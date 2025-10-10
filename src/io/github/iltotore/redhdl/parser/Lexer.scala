package io.github.iltotore.redhdl.parser

import io.github.iltotore.redhdl.ast.Identifier
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
    "input" -> Token.Input,
    "output" -> Token.Output,
    "begin" -> Token.Begin,
    "end" -> Token.End
  )

  val parseTerm: Token < Parse[Char] = Parse.firstOf(
    Parse.boolean.map(Token.LBool.apply),
    Parse.inOrder(
      Parse.identifier,
      Parse.literal('.'),
      Parse.identifier
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

  val parseAnyToken: Token < Parse[Char] = Parse.firstOf(
    parseSymbol,
    parseKeyword,
    parseTerm
  )

  val parseTokens: Chunk[Token] < Parse[Char] = 
    Parse.entireInput(Parse.spaced(Parse.repeat(parseAnyToken)))