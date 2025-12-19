package io.github.iltotore.redhdl

import kyo.*
import io.github.iltotore.redhdl.ast.Program
import io.github.iltotore.redhdl.parser.Lexer
import io.github.iltotore.redhdl.parser.Parser
import io.github.iltotore.redhdl.typer.Typing
import io.github.iltotore.redhdl.typer.TypeChecker
import io.github.iltotore.redhdl.typer.ComponentInfo
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ir.Expansion
import io.github.iltotore.redhdl.ir.Expander
import io.github.iltotore.redhdl.ir.ExpandedComponent
import io.github.iltotore.redhdl.ir.Simplifier
import io.github.iltotore.redhdl.ir.SimplifiedComponent
import io.github.iltotore.redhdl.graph.GraphBuilder
import io.github.iltotore.redhdl.graph.GraphBuilding
import io.github.iltotore.redhdl.graph.Graph
import io.github.iltotore.redhdl.graph.NodeId
import io.github.iltotore.redhdl.graph.GraphLayout

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

def compileToGraph(entrypoint: Identifier, components: Map[Identifier, ComponentInfo]): Graph =
  Expander
    .expandComponent(components(entrypoint))
    .map(Simplifier.simplifyComponent)
    .map(GraphBuilding.buildGraph)
    .handle(Expansion.run(components)).eval

def compileToSchem(graph: Graph): Chunk[Chunk[NodeId]] =
  GraphLayout.getLayers(graph)