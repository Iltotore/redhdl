package io.github.iltotore.redhdl

import kyo.*
import io.github.iltotore.redhdl.graph.Channel
import scala.util.Using
import scala.io.Source
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.graph.Graph
import io.github.iltotore.redhdl.graph.GraphRouter
import io.github.iltotore.redhdl.graph.NodeId
import io.github.iltotore.redhdl.graph.NetId
import io.github.iltotore.redhdl.graph.Net

object Main extends KyoApp:
  
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
      .flatMap(id => Chunk.fill(graph.getNode(id).tpe.width)(Integer.toHexString(id.value).toUpperCase))
      .mkString(" ")

    val width = layerSize * 2 - 1
    val height = channel.tracks.size * 4 + 1

    val grid = Array.fill(height)(Array.fill(width)(" "))

    def drawNet(from: Int, net: Net, id: NetId, color: String): Unit =
      val netStartX = net.start.value * 2
      val netEndX = net.end.value * 2
      val trackZ = channel.getNetTrack(id).value * 4 + 1
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

  run:
    direct:
      val code = Using.resource(Source.fromFile("test/resources/golden/good/circuits/fullAdder.red"))(_.mkString)

      val typeResult = typecheck(code)
      Console.printLine(typeResult).now
      Console.printLine("=" * 30).now

      typeResult match
        case Result.Success(components) =>
          val initialGraph = compileToGraph(Identifier("FullAdder"), components)
          val initialLayers = GraphRouter.getLayers(initialGraph)
          val (graph, layers) = GraphRouter.addRelays(initialGraph, initialLayers)
          Console.printLine(graph).now
          Console.printLine("=" * 30).now
          Console.printLine(layers).now
          val channels = compileToSchem(graph, layers)
          val layerSize = channels.map(_.width).max
          // Console.printLine(channels).now
          Console.printLine(layers.zip(channels).map(showChannel(graph, layerSize, _, _)).mkString("\n")).now
          Console.printLine(layers.last.mkString(" ")).now
        case _ => Console.printLine(typeResult).now
    