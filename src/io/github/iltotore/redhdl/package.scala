package io.github.iltotore.redhdl

import kyo.*
import io.github.iltotore.redhdl.ast.untpd.Program
import io.github.iltotore.redhdl.parser.Lexer
import io.github.iltotore.redhdl.parser.Parser

def parse(code: String): ParseResult[Program] =
  direct:
    val lexResult = Parse.runResult(code)(Lexer.parseTokens).now
    lexResult.out match
      case Absent => ParseResult(lexResult.errors, Absent, lexResult.fatal)
      case Present(tokens) =>
        val parseResult = Parse.runResult(tokens)(Parser.parseProgram).now
        parseResult.copy(errors = lexResult.errors ++ parseResult.errors)
  .eval