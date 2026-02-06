package io.github.iltotore.redhdl

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Program
import io.github.iltotore.redhdl.graph.Channel
import io.github.iltotore.redhdl.graph.Graph
import io.github.iltotore.redhdl.graph.GraphBuilder
import io.github.iltotore.redhdl.graph.GraphBuilding
import io.github.iltotore.redhdl.graph.GraphRouter
import io.github.iltotore.redhdl.graph.NodeId
import io.github.iltotore.redhdl.ir.ExpandedComponent
import io.github.iltotore.redhdl.ir.Expander
import io.github.iltotore.redhdl.ir.Expansion
import io.github.iltotore.redhdl.ir.SimplifiedComponent
import io.github.iltotore.redhdl.ir.Simplifier
import io.github.iltotore.redhdl.minecraft.GateType
import io.github.iltotore.redhdl.minecraft.SchematicContext
import io.github.iltotore.redhdl.minecraft.SchematicGeneration
import io.github.iltotore.redhdl.minecraft.SchematicGenerator
import io.github.iltotore.redhdl.minecraft.Structure
import io.github.iltotore.redhdl.parser.Lexer
import io.github.iltotore.redhdl.parser.Parser
import io.github.iltotore.redhdl.typer.ComponentInfo
import io.github.iltotore.redhdl.typer.TypeChecker
import io.github.iltotore.redhdl.typer.Typing
import kyo.*
import kyo.Channel as KyoChannel
import io.github.iltotore.redhdl.typer.TypeFailure

def parse(code: String): Program < Compilation =
  Compilation
    .fromParseResult(Parse.runResult(code)(Lexer.parseTokens))
    .map(tokens => Compilation.fromParseResult(Parse.runResult(tokens)(Parser.parseProgram)))

def typecheck(program: Program): Map[Identifier, ComponentInfo] < Compilation =
  Typing.runGlobal(TypeChecker.checkProgram(program))

def compileToGraph(entrypoint: Identifier, components: Map[Identifier, ComponentInfo]): Graph =
  Expander
    .expandComponent(components(entrypoint))
    .map(Simplifier.simplifyComponent)
    .map(GraphBuilding.buildGraph)
    .handle(Expansion.run(components)).eval

def compileToSchem(graph: Graph, layers: Chunk[Chunk[NodeId]], context: SchematicContext): Structure < Compilation =
  SchematicGeneration.run(context)(SchematicGenerator.generateStructure(graph, layers, GraphRouter.routeGraph(graph, layers)))

def compileRedHDL(code: String): Structure < Compilation =
  for
    fileName <- CompilationContext.fileName
    entrypoint <- CompilationContext.entrypoint
    components <- parse(code).map(typecheck)

    resolvedEntrypoint =
      entrypoint
        .orElse(fileName.flatMap(name => Maybe.fromOption(components.keys.find(_.value.equalsIgnoreCase(name)))))
        .getOrElse(Identifier("Main"))

    _ <-
      if components.contains(resolvedEntrypoint) then Kyo.unit
      else Compilation.emitAndAbort(TypeFailure.UnknownEntrypoint(resolvedEntrypoint))

    initialGraph = compileToGraph(resolvedEntrypoint, components)
    initialLayers = GraphRouter.getLayers(initialGraph)
    (graph, layers) = GraphRouter.addRelays(initialGraph, initialLayers)
    schemContext <- Abort.recover(Compilation.emitAndAbort)(SchematicContext.load(GateType.values))
  yield compileToSchem(graph, layers, schemContext)
