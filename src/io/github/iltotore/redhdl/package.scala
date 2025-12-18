package io.github.iltotore.redhdl

import kyo.*
import io.github.iltotore.redhdl.ast.Program
import io.github.iltotore.redhdl.parser.Lexer
import io.github.iltotore.redhdl.parser.Parser
import io.github.iltotore.redhdl.typer.Typing
import io.github.iltotore.redhdl.typer.TypeChecker
import io.github.iltotore.redhdl.typer.ComponentInfo
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.graph.Expansion
import io.github.iltotore.redhdl.graph.Expander
import io.github.iltotore.redhdl.graph.ExpandedComponent

def parse(code: String): ParseResult[Program] =
  direct:
    val lexResult = Parse.runResult(code)(Lexer.parseTokens).now
    lexResult.out match
      case Absent => ParseResult(lexResult.errors, Absent, lexResult.fatal)
      case Present(tokens) =>
        val parseResult = Parse.runResult(tokens)(Parser.parseProgram).now
        parseResult.copy(errors = lexResult.errors ++ parseResult.errors)
  .eval

def typecheck(code: String): Result[Chunk[CompilerFailure], Map[Identifier, ComponentInfo]] =
  val parsed = parse(code)
  parsed.out match
    case Absent => Result.Failure(parsed.errors)
    case Present(program) =>
      Typing.runGlobal(TypeChecker.checkProgram(program))
        .eval
        .mapFailure(parsed.errors ++ _)

def compile(entrypoint: Identifier, components: Map[Identifier, ComponentInfo]): ExpandedComponent =
  Expansion.run(components)(Expander.expandComponent(components(entrypoint))).eval