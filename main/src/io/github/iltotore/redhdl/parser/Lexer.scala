package io.github.iltotore.redhdl.parser

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.parser.debug
import kyo.*

/**
 * The lexical analysis phase of the RedHDL compiler.
 *
 * [[Lexer]] converts a stream of characters into a flat sequence of [[Token]]s.
 * It is built on top of the Kyo `Parse` combinator library and is structured as
 * a set of composable parsers that are combined bottom-up into the public entry
 * point [[parseTokens]].
 *
 * Lexer errors are recovered from using [[kyo.RecoverStrategy.skipThenRetryUntil]],
 * which skips the offending character and retries, so as many tokens as possible are
 * produced even in the presence of invalid input.
 */
object Lexer:

  /**
   * Map of single-character (or multi-character) symbol strings to their [[Token]]
   * counterparts.  Entries are tried longest-first during parsing.
   */
  val symbols: Map[String, Token] = Map(
    "(" -> Token.ParenOpen,
    ")" -> Token.ParenClosed,
    "," -> Token.Comma,
    ":" -> Token.Colon,
    "=" -> Token.Equal
  )

  /**
   * Map of reserved keyword strings to their [[Token]] counterparts.
   */
  val keywords: Map[String, Token] = Map(
    "component" -> Token.Component,
    "subcomponent" -> Token.Subcomponent,
    "input" -> Token.Input,
    "output" -> Token.Output,
    "begin" -> Token.Begin,
    "end" -> Token.End,
    "not" -> Token.Not,
    "or" -> Token.Or,
    "and" -> Token.And,
    "xor" -> Token.Xor
  )

  /**
   * Parse a single Boolean literal, a dotted sub-component identifier
   * (`sub.port`), or a plain main-component identifier.
   *
   * The alternatives are tried in order; the first match wins.
   */
  val parseTerm: Token < Parse[Char] = Parse.firstOf(
    Parse.boolean.map(Token.LBool.apply),
    Parse.inOrder(
      Parse.identifier,
      Parse.literal('.'),
      Parse.require(withErrorMessage(Parse.identifier, "Identifier expected after `.`"))
    ).map((sub, _, name) => Token.SubIdent(Identifier.assume(sub.toString), Identifier.assume(name.toString))),
    Parse.identifier.map(id => Token.MainIdent(Identifier.assume(id.toString)))
  )

  /**
   * Parse one symbol token, trying all [[symbols]] entries longest-first.
   */
  val parseSymbol: Token < Parse[Char] =
    Parse.firstOf(
      symbols
        .toList
        .sortBy((k, v) => -k.length)
        .map((k, v) => () => Parse.literal(k).andThen(v))
    )

  /**
   * Parse an identifier and look it up in the [[keywords]] map.
   *
   * If the identifier is not a keyword the parser fails, allowing [[parseAnyToken]]
   * to fall through to [[parseTerm]].
   */
  val parseKeyword: Token < Parse[Char] =
    Parse.identifier.map(kw =>
      keywords.get(kw.toString) match
        case Some(token) => token
        case None        => Parse.fail("Invalid keyword")
    )

  /**
   * Parse any single token, recovering from unrecognised input by skipping one
   * character and retrying.
   *
   * The parse order is: symbol → keyword → term.  Any character sequence that does
   * not match is skipped via [[kyo.RecoverStrategy.skipThenRetryUntil]] and an error
   * is recorded.
   */
  val parseAnyToken: Token < Parse[Char] =
    val parser = Parse.firstOf(
      parseSymbol,
      parseKeyword,
      parseTerm
    )

    Parse.recoverWith(
      Parse.require(withErrorMessage(parser, "Invalid token")),
      RecoverStrategy.skipThenRetryUntil(Parse.any, parser)
    )

  /**
   * Parse an entire source file into a flat sequence of [[Token]]s.
   *
   * Leading and trailing whitespace is consumed by [[Parse.spaced]].  Parsing
   * continues until the end of input is reached.
   */
  val parseTokens: Chunk[Token] < Parse[Char] =
    Parse.spaced(Parse.repeatUntil(parseAnyToken, Parse.end))
