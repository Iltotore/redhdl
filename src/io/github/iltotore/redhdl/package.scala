package io.github.iltotore.redhdl

import kyo.*
import io.github.iltotore.redhdl.ast.Program
import io.github.iltotore.redhdl.parser.Lexer
import io.github.iltotore.redhdl.parser.Parser
import io.github.iltotore.redhdl.typer.Typing
import io.github.iltotore.redhdl.typer.TypeChecker

def parse(code: String): ParseResult[Program] =
  direct:
    val lexResult = Parse.runResult(code)(Lexer.parseTokens).now
    lexResult.out match
      case Absent => ParseResult(lexResult.errors, Absent, lexResult.fatal)
      case Present(tokens) =>
        val parseResult = Parse.runResult(tokens)(Parser.parseProgram).now
        parseResult.copy(errors = lexResult.errors ++ parseResult.errors)
  .eval

def compile(code: String): Result[Chunk[CompilerFailure], Program] =
  val parsed = parse(code)
  parsed.out match
    case Absent => Result.Failure(parsed.errors)
    case Present(program) =>
      Typing.runGlobal(TypeChecker.checkProgram(program).andThen(program))
        .eval
        .map(_._2)
        .mapFailure(parsed.errors ++ _)