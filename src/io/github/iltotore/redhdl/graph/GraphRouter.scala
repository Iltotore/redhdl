package io.github.iltotore.redhdl.graph

import kyo.Chunk
import kyo.Absent
import kyo.Present
import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.annotation.nowarn

object GraphRouter:

  /**
   * Get graph's layers using Kahn's algorithm.
   *
   * @param graph the graph to sort
   * @return the graph's topological layers
   */
  def getLayers(graph: Graph): Chunk[Chunk[NodeId]] =
    val inputDegree = mutable.Map[NodeId, Int]().withDefaultValue(0)

    for (node, id) <- graph.nodes.zipWithIndex do
      inputDegree.getOrElseUpdate(NodeId.assume(id), 0): Unit
      node.outputs.foreach(output => inputDegree(output.id) += 1)

    val layers = mutable.ListBuffer[Chunk[NodeId]]()
    var zeroInputDegree = inputDegree
      .collect:
        case (id, 0) => id
      .toSet

    while zeroInputDegree.nonEmpty do
      layers += Chunk.from(zeroInputDegree)

      val nextZero = mutable.Set[NodeId]()

      for id <- zeroInputDegree do
        for output <- graph.getOutputs(id) do
          inputDegree(output.id) -= 1
          if (inputDegree(output.id) == 0) nextZero += output.id

        inputDegree.remove(id)

      zeroInputDegree = nextZero.toSet

    if inputDegree.nonEmpty then
      throw new AssertionError("Graph contains a cycle")

    Chunk.from(layers)

  //Might just be replaced by a zipWithIndex in getChannel in the future
  def getXPositions(graph: Graph, layers: Chunk[Chunk[NodeId]]): Map[NodeOutput, PinX] =
    val positions = mutable.Map.empty[NodeOutput, PinX]

    for layer <- layers do
      var x = PinX(0)
      for
        id <- layer
        input <- 0 until graph.getNode(id).tpe.width
      do
        positions(NodeOutput(id, input)) = x
        x += 1

    positions.toMap

  //See https://github.com/itsfrank/MinecraftHDL/blob/c66690ae2f1ee2b04aae214a694eb6fe0e03d326/src/main/java/minecrafthdl/synthesis/routing/Router.java#L91
  def createChannel(graph: Graph, xPos: Map[NodeOutput, PinX], from: Chunk[NodeId], to: Chunk[NodeId]): Channel =
    Channel(
      from
        .map(fromId =>
          val netOutputs = graph.getOutputs(fromId)
          val positions = netOutputs.map(xPos)
          Net(xPos(NodeOutput(fromId, 0)), positions.foldLeft(PinX(0))(_ max _))
        ),
      Chunk.empty
    )

  def assignTrack(channel: Channel, netId: NetId): Channel =
    val net = channel.nets(netId.value)
    val availableTrack = channel.tracks.zipWithIndex.find((track, _) => channel.getTrackEnd(track) < net.left)
    availableTrack match
      case None =>
        channel.copy(
          tracks =
            channel
              .tracks
              .appended(Track(Chunk(netId)))
        )

      case Some((track, trackId)) =>
        channel.copy(
          tracks =
            channel
              .tracks
              .updated(trackId, track.copy(nets = track.nets :+ netId))
        )

  def routeChannel(channel: Channel): Channel =
    NetId
      .assumeAll(Chunk.range(0, channel.nets.size))
      .foldLeft(channel)(assignTrack)

  // https://rtldigitaldesign.blogspot.com/2019/07/left-edge-channel-algorithm-for.html
  @nowarn("msg=exhaustive")
  def routeGraph(graph: Graph, layers: Chunk[Chunk[NodeId]]): Chunk[Channel] =
    val xPos = getXPositions(graph, layers)
    println(pprint(xPos))
    Chunk.from(
      layers.sliding(2).map:
        case Chunk(from, to) =>
          val channel = createChannel(graph, xPos, from, to)
          routeChannel(channel)
    )

