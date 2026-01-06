package io.github.iltotore.redhdl.graph

import kyo.Chunk
import kyo.Absent
import kyo.Present
import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.annotation.nowarn
import kyo.Maybe

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
      for
        nodeId <- from
        outputPositions = graph.getOutputs(nodeId).map(xPos.apply)
        if !outputPositions.isEmpty
      yield
        Net(xPos(NodeOutput(nodeId, 0)), outputPositions.max, Absent),
      Chunk.empty,
      Absent
    )

  def hasCycleAt(channel: Channel, done: Set[NetId], at: Net): Boolean =
    def rec(current: PinX): Boolean =
      val (id, net) = channel.getNetAt(current)
      if net.start == net.end || done.contains(id) then false
      else if current == at.start then true
      else rec(net.end)

    rec(at.end)

  /**
   * Break the cycle if it exists at the given net by creating an out column/net 
   */
  def breakCycle(channel: Channel, done: Set[NetId], netId: NetId): (Channel, Set[NetId]) =
    val net = channel.getNet(netId)
    if hasCycleAt(channel, done, net) then (channel.reroute(netId), done + netId)
    else (channel, done + netId)

  /*
  0 1 2 3
  +   | |
  --- | |
    + | |
    | | |
    | | +
  -------
  + | |
  0 1 2

  Net0: left = 0, right = 1
  Net1: left = 2, right = 2
  Net2: left = 0, right = 3

  Pour Net0:
    - Track 0: [Net0] (end = 1)

  Pour Net1:
    - Track 0 (car Net1.left > Track0.end): [Net0, Net1] (end = 2)

  Pour Net2:
    - Track 0 (car Net1.left > Track0.end): [Net0, Net1] (end = 2)
    - Track 1: [Net2] (end = 3)
  */
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
    val sortedNets = Chunk
      .range(NetId(0), NetId.assume(channel.nets.size))
      .sortBy(id => 
        val net = channel.getNet(id)
        (net.left, net.right)
      )

    val (withoutCycle, _) = sortedNets.foldLeft((channel, Set.empty[NetId])):
      case ((channel, done), id) =>
        breakCycle(channel, done, id)

    val sortedNetsThenOuters = sortedNets ++ Chunk.range(NetId.assume(sortedNets.size), NetId.assume(withoutCycle.nets.size))
    sortedNetsThenOuters.foldLeft(withoutCycle)(assignTrack)

  // https://rtldigitaldesign.blogspot.com/2019/07/left-edge-channel-algorithm-for.html
  @nowarn("msg=exhaustive")
  def routeGraph(graph: Graph, layers: Chunk[Chunk[NodeId]]): Chunk[Channel] =
    val xPos = getXPositions(graph, layers)
    Chunk.from(
      layers.sliding(2).map:
        case Chunk(from, to) =>
          val channel = createChannel(graph, xPos, from, to)
          routeChannel(channel)
    )

/*
A: colonne 0-3
B: colonne 1-4
C: colonne 2-5

Pour Swap3:

A -> B
B -> C
C -> A

aka A -> B -> C -> A

Si on brise le cycle:

A -> D -> B
B -> C -> A
*/