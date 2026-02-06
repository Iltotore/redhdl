package io.github.iltotore.redhdl.graph

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import scala.annotation.nowarn
import scala.annotation.threadUnsafe
import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps

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

  def addRelays(graph: Graph, layers: Chunk[Chunk[NodeId]]): (Graph, Chunk[Chunk[NodeId]]) =
    var updatedGraph = graph
    var updatedLayers = layers

    def getLayerOf(id: NodeId): Int =
      updatedLayers
        .zipWithIndex
        .collectFirst:
          case (layer, y) if layer.contains(id) => y
        .get

    for
      (layer, y) <- layers.zipWithIndex
      id <- layer
      (output, outputIndex) <- updatedGraph.getNode(id).outputs.zipWithIndex
      targetY = getLayerOf(output.id)
      if targetY > y + 1
    do
      val node = updatedGraph.getNode(id)
      val nextId = updatedGraph.nodes.size
      val numberOfRelays = targetY - (y + 1)
      val nextIdAfterRelays = nextId + numberOfRelays
      val relayOutputs = Chunk
        .range(nextIdAfterRelays - 1, nextId - 1, -1)
        .map(id => NodeOutput(NodeId.assume(id), 0))

      val nodes = relayOutputs
        .reverse
        .prepended(output)
        .init
        .map(out => Node(NodeType.Relay, Chunk(out)))

      val updatedNodeOutputs = node.outputs.updated(outputIndex, relayOutputs.head)

      updatedGraph = updatedGraph.copy(nodes =
        updatedGraph
          .nodes
          .updated(id.value, node.copy(outputs = updatedNodeOutputs))
          .appendedAll(nodes)
      )

      for (relay, y) <- relayOutputs.zip(y + 1 until targetY) do
        updatedLayers = updatedLayers.updated(y, updatedLayers(y) :+ relay.id)

    (updatedGraph, updatedLayers)

  // Might just be replaced by a zipWithIndex in getChannel in the future
  def getXPositions(graph: Graph, layers: Chunk[Chunk[NodeId]]): Map[NodeOutput, PinX] =
    val positions = mutable.Map.empty[NodeOutput, PinX]

    for layer <- layers do
      var x = PinX(0)
      for
        id <- layer
        input <- 0 until graph.getNode(id).tpe.sizeX
      do
        positions(NodeOutput(id, input)) = x
        x += 1

    positions.toMap

  // See https://github.com/itsfrank/MinecraftHDL/blob/c66690ae2f1ee2b04aae214a694eb6fe0e03d326/src/main/java/minecrafthdl/synthesis/routing/Router.java#L91
  def createChannel(graph: Graph, xPos: Map[NodeOutput, PinX], from: Chunk[NodeId], to: Chunk[NodeId]): Channel =
    Channel(
      for
        nodeId <- from
        outputPos <- graph.getOutputs(nodeId).map(xPos.apply)
      yield Net(xPos(NodeOutput(nodeId, 0)), outputPos, Absent),
      Chunk.empty,
      Absent
    )

  def hasCycleAt(channel: Channel, done: Set[NetId], at: Net): Boolean =
    def rec(current: PinX): Boolean =
      channel.getNetAt(current).exists((id, net) =>
        if net.start == net.end || done.contains(id) then false
        else if current == at.start then true
        else rec(net.end)
      )

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
    val availableTrack = channel
      .tracks
      .zipWithIndex.find((track, trackId) =>
        val hasSameStart = track.nets
          .map(channel.getNet)
          .exists(_.start == net.start) 

        val isTrackBeforeDest = channel
          .getNetAt(net.end)
          .flatMap((id, _) => channel.getNetTrack(id))
          .exists(destTrackId => trackId <= destTrackId.value)

        (channel.getTrackEnd(track) < net.left || hasSameStart) && !isTrackBeforeDest
      )
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

  def sortNets(channel: Channel): Chunk[NetId] =
    val innerNets = channel
      .nets
      .zipWithIndex
      .filterNot((net, _) => channel.isOuterColumn(net.start))

    val degrees = innerNets
      .groupBy(_._1.start)
      .map((x, nets) => (x, nets.size))
      .to(mutable.Map)

    val queue = innerNets.to(mutable.Queue)
    val result = mutable.ListBuffer.empty[NetId]

    while !queue.isEmpty do
      val (net, id) = queue.dequeue()

      if degrees.getOrElse(net.end, 0) == 0 || net.start == net.end then
        result += NetId.assume(id)
        degrees(net.start) -= 1
      else
        queue.enqueue((net, id))

    result.to(Chunk)

  def routeChannel(channel: Channel): Channel =
    val (withoutCycle, _) = Chunk
      .range(NetId(0), NetId.assume(channel.nets.size))
      .foldLeft((channel, Set.empty[NetId])):
        case ((channel, done), id) =>
          breakCycle(channel, done, id)

    val sortedNets = sortNets(withoutCycle)

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
