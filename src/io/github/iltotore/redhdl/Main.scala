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
    val headLine = layerFrom
      .flatMap(id => Chunk.fill(graph.getNode(id).tpe.width)(id))
      .mkString(" ")

    val width = layerSize * 2 - 1
    val height = channel.tracks.size * 4 + 1

    val grid = Array.fill(height)(Array.fill(width)(' '))

    def drawNet(from: Int, net: Net, id: NetId): Unit =
      val netStartX = net.start.value * 2
      val netEndX = net.end.value * 2
      val trackZ = channel.getNetTrack(id).value * 4 + 1
      
      for
        z <- from until trackZ - 1
        if grid(z)(netStartX) == ' '
      do
        grid(z)(netStartX) = '|'

      if netStartX == netEndX then
        grid(trackZ - 1)(netStartX) = '|'
        grid(trackZ)(netStartX) = '|'
        grid(trackZ + 1)(netStartX) = '|'
      else
        grid(trackZ - 1)(netStartX) = '+'
        for x <- math.min(netStartX, netEndX) to math.max(netStartX, netEndX) do
          grid(trackZ)(x) = '-'
        grid(trackZ + 1)(netEndX) = '+'

      net.outerNet match
        case Absent =>
          for
            z <- trackZ + 2 until height
            if grid(z)(netEndX) == ' '
          do grid(z)(netEndX) = '|'
        
        case Present(outerId) =>
          drawNet(trackZ + 2, channel.getNet(outerId), outerId)
    end drawNet

    for (net, id) <- channel.nets.zipWithIndex if !channel.isOuterColumn(net.start) do
      drawNet(0, net, NetId.assume(id))
    
    s"$headLine\n${grid.map(_.mkString).mkString("\n")}"

  run:
    direct:
      val code = Using.resource(Source.fromFile("test/resources/golden/good/swap3.red"))(_.mkString)

      val typeResult = typecheck(code)
      Console.printLine(typeResult).now
      Console.printLine("=" * 30).now

      typeResult match
        case Result.Success(components) =>
          val graph = compileToGraph(Identifier("Swap3"), components)
          Console.printLine(graph).now
          val layers = GraphRouter.getLayers(graph)
          val channels = compileToSchem(graph, layers)
          val layerSize = channels.map(_.width).max
          Console.printLine(channels).now
          Console.printLine(layers.zip(channels).map(showChannel(graph, layerSize, _, _)).mkString("\n")).now
          Console.printLine(layers.last.mkString(" ")).now
        case _ => Console.printLine(typeResult).now
    