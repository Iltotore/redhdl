package io.github.iltotore.redhdl

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Program
import io.github.iltotore.redhdl.graph.Channel
import io.github.iltotore.redhdl.graph.Graph
import io.github.iltotore.redhdl.graph.GraphBuilder
import io.github.iltotore.redhdl.graph.GraphBuilding
import io.github.iltotore.redhdl.graph.GraphRouter
import io.github.iltotore.redhdl.graph.Net
import io.github.iltotore.redhdl.graph.NetId
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
import io.github.iltotore.redhdl.typer.TypeFailure
import io.github.iltotore.redhdl.typer.Typing
import kyo.*
import kyo.Channel as KyoChannel

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

def idToChar(id: NodeId): Char =
  if id.value >= 26 then ('a' + id.value - 26).toChar
  else ('A' + id.value).toChar

/*
String representation of the circuit
 */
private def showChannel(graph: Graph, layerSize: Int, layerFrom: Chunk[NodeId], channel: Channel): String =
  val colors = Chunk(
    scala.Console.RED,
    scala.Console.GREEN,
    scala.Console.BLUE,
    scala.Console.MAGENTA,
    scala.Console.YELLOW,
    scala.Console.CYAN
  )

  val headLine = layerFrom
    .flatMap(id => Chunk.fill(graph.getNode(id).tpe.sizeX)(idToChar(id)))
    .mkString(" ")

  val width = layerSize * 2 - 1
  val height = channel.tracks.size * 4 + 1

  val grid = Array.fill(height)(Array.fill(width)(" "))

  def drawNet(from: Int, net: Net, id: NetId, color: String): Unit =
    val netStartX = net.start.value * 2
    val netEndX = net.end.value * 2
    val trackZ = channel.getNetTrack(id).get.value * 4 + 1
    def colored(str: String): String = s"$color$str${scala.Console.RESET}"

    for
      z <- from until trackZ - 1
      if grid(z)(netStartX) == " "
    do
      grid(z)(netStartX) = colored("|")

    if netStartX == netEndX then
      grid(trackZ - 1)(netStartX) = colored("|")
      grid(trackZ)(netStartX) = colored("|")
      grid(trackZ + 1)(netStartX) = colored("|")
    else
      grid(trackZ - 1)(netStartX) = colored("+")
      for x <- math.min(netStartX, netEndX) to math.max(netStartX, netEndX) do
        grid(trackZ)(x) = colored("-")
      grid(trackZ + 1)(netEndX) = colored("+")

    net.outerNet match
      case Absent =>
        for
          z <- trackZ + 2 until height
          if grid(z)(netEndX) == " "
        do grid(z)(netEndX) = colored("|")

      case Present(outerId) =>
        drawNet(trackZ + 2, channel.getNet(outerId), outerId, color)
  end drawNet

  for (net, id) <- channel.nets.zipWithIndex if !channel.isOuterColumn(net.start) do
    drawNet(0, net, NetId.assume(id), colors(net.start.value % colors.size))

  s"$headLine\n${grid.map(_.mkString).mkString("\n")}"
