package io.github.iltotore.redhdl.parser

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.parser.debug
import kyo.*

object Lexer:

  val symbols: Map[String, Token] = Map(
    "(" -> Token.ParenOpen,
    ")" -> Token.ParenClosed,
    "," -> Token.Comma,
    ":" -> Token.Colon,
    "=" -> Token.Equal
  )

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
  )

  val parseTerm: Token < Parse[Char] = Parse.firstOf(
    Parse.boolean.map(Token.LBool.apply),
    Parse.inOrder(
      Parse.identifier,
      Parse.literal('.'),
      Parse.require(withErrorMessage(Parse.identifier, "Identifier expected after `.`")),
    ).map((sub, _, name) => Token.SubIdent(Identifier.assume(sub.toString), Identifier.assume(name.toString))),
    Parse.identifier.map(id => Token.MainIdent(Identifier.assume(id.toString)))
  )

  val parseSymbol: Token < Parse[Char] =
    Parse.firstOf(
      symbols
        .toList
        .sortBy((k, v) => -k.length)
        .map((k, v) => () => Parse.literal(k).andThen(v))
    )

  val parseKeyword: Token < Parse[Char] =
    Parse.identifier.map(kw =>
      keywords.get(kw.toString) match
        case Some(token) => token
        case None        => Parse.fail("Invalid keyword")
    )

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

  val parseTokens: Chunk[Token] < Parse[Char] = 
    Parse.spaced(Parse.repeatUntil(parseAnyToken, Parse.end))