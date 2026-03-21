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

/**
 * Lex and parse a RedHDL source string into a [[Program]] AST.
 *
 * The function runs two parse passes in sequence: the [[Lexer]] converts the raw
 * character stream into a [[kyo.Chunk]] of [[io.github.iltotore.redhdl.parser.Token]]s,
 * and the [[Parser]] converts those tokens into a [[Program]].  Both passes use
 * [[Compilation.fromParseResult]] so that parse errors are emitted as
 * [[CompilerFailure]] diagnostics and a missing result triggers an abort.
 *
 * @param code The complete source text to parse.
 * @return The parsed [[Program]] within the [[Compilation]] effect.
 */
def parse(code: String): Program < Compilation =
  Compilation
    .fromParseResult(Parse.runResult(code)(Lexer.parseTokens))
    .map(tokens => Compilation.fromParseResult(Parse.runResult(tokens)(Parser.parseProgram)))

/**
 * Run the type-checking pass on a parsed [[Program]].
 *
 * Delegates to [[TypeChecker.checkProgram]] inside [[Typing.runGlobal]], which
 * returns the map of all successfully checked components.
 *
 * @param program The AST produced by [[parse]].
 * @return A map from component [[Identifier]] to [[ComponentInfo]], within
 *         the [[Compilation]] effect.
 */
def typecheck(program: Program): Map[Identifier, ComponentInfo] < Compilation =
  Typing.runGlobal(TypeChecker.checkProgram(program))

/**
 * Compile a named entry-point component into a logic [[Graph]].
 *
 * The pipeline is:
 *   1. [[Expander.expandComponent]] – inline all sub-component instantiations.
 *   2. [[Simplifier.simplifyComponent]] – eliminate internal wires and optionally
 *      apply algebraic optimisations.
 *   3. [[GraphBuilding.buildGraph]] – translate the simplified expression tree into a
 *      DAG of logic nodes.
 *
 * @param entrypoint The name of the component to compile.
 * @param components The full component map produced by [[typecheck]].
 * @param optimize   Whether to apply constant-folding and algebraic simplifications.
 * @return The completed logic [[Graph]].
 */
def compileToGraph(entrypoint: Identifier, components: Map[Identifier, ComponentInfo], optimize: Boolean): Graph =
  Expander
    .expandComponent(components(entrypoint))
    .map(Simplifier.simplifyComponent(optimize))
    .map(GraphBuilding.buildGraph)
    .handle(Expansion.run(components)).eval

/**
 * Route a logic graph and generate a Minecraft [[Structure]] from it.
 *
 * @param graph    The logic graph (with relay nodes already inserted).
 * @param layers   The layered partition of the graph.
 * @param context  The [[SchematicContext]] containing gate schematics and palette.
 * @param optimize Whether the optimised channel-routing heuristic is enabled.
 * @return The generated [[Structure]], within the [[Compilation]] effect.
 */
def compileToSchem(graph: Graph, layers: Chunk[Chunk[NodeId]], context: SchematicContext, optimize: Boolean): Structure < Compilation =
  SchematicGeneration.run(context)(SchematicGenerator.generateStructure(graph, layers, GraphRouter.routeGraph(graph, layers, optimize)))

/**
 * Full compilation pipeline: source text → Minecraft [[Structure]].
 *
 * Reads the compilation settings from the ambient [[CompilationContext]] and runs
 * all stages in order:
 *   1. Parsing ([[parse]]).
 *   2. Type checking ([[typecheck]]).
 *   3. Entry-point resolution.
 *   4. Graph construction ([[compileToGraph]]).
 *   5. Relay insertion ([[GraphRouter.addRelays]]).
 *   6. Schematic loading and generation ([[compileToSchem]]).
 *
 * @param code The complete RedHDL source text to compile.
 * @return The compiled [[Structure]] within the [[Compilation]] effect, or an
 *         abort with accumulated [[CompilerFailure]] diagnostics on failure.
 */
def compileRedHDL(code: String): Structure < Compilation =
  for
    fileName <- CompilationContext.fileName
    entrypoint <- CompilationContext.entrypoint
    optimize <- CompilationContext.optimize
    alignOutputs <- CompilationContext.alignOutputs
    components <- parse(code).map(typecheck)

    resolvedEntrypoint =
      entrypoint
        .orElse(fileName.flatMap(name => Maybe.fromOption(components.keys.find(_.value.equalsIgnoreCase(name)))))
        .getOrElse(Identifier("Main"))

    _ <-
      if components.contains(resolvedEntrypoint) then Kyo.unit
      else Compilation.emitAndAbort(TypeFailure.UnknownEntrypoint(resolvedEntrypoint))

    initialGraph = compileToGraph(resolvedEntrypoint, components, optimize)
    initialLayers = GraphRouter.getLayers(initialGraph, alignOutputs)
    (graph, layers) = GraphRouter.addRelays(initialGraph, initialLayers)
    // load schematics and propagate current palette and optimize configuration
    pal <- CompilationContext.palette
    delay <- CompilationContext.repeaterDelay
    schemContext <- Abort.recover(Compilation.emitAndAbort)(SchematicContext.load(GateType.values, pal, delay))
  yield compileToSchem(graph, layers, schemContext, optimize)

/**
 * Map a [[NodeId]] to a printable character for debug display.
 *
 * IDs 0–25 map to uppercase letters A–Z; IDs 26 and above map to lowercase
 * letters a–z.  This is used when rendering a textual circuit diagram.
 *
 * @param id The node identifier to convert.
 * @return A single character representing the node.
 */
def idToChar(id: NodeId): Char =
  if id.value >= 26 then ('a' + id.value - 26).toChar
  else ('A' + id.value).toChar

/*
String representation of the circuit
 */
/**
 * Render a routing [[Channel]] as a coloured ASCII diagram for debugging.
 *
 * Each net is drawn as a column of vertical bars and a horizontal bridge at the
 * net's assigned track row.  ANSI colour codes are applied to make individual
 * nets visually distinct.  The diagram is preceded by a header line showing the
 * node characters of the source layer.
 *
 * @param graph     The logic graph (used to determine node widths).
 * @param layerSize The total pin-column width of the source layer.
 * @param layerFrom The [[NodeId]]s in the source layer.
 * @param channel   The [[Channel]] to render.
 * @return A multi-line string containing the ASCII diagram.
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
