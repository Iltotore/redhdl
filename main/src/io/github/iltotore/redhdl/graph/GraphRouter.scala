package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.CompilationContext.optimize
import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import scala.annotation.nowarn
import scala.annotation.threadUnsafe
import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps

/**
 * Graph routing algorithms used to convert a logic [[Graph]] into a set of
 * [[Channel]]s that can be directly translated into a Minecraft schematic.
 *
 * The overall pipeline is:
 *   1. [[getLayers]]     – topological sort into layers (Kahn's algorithm).
 *   2. [[addRelays]]     – insert relay nodes for edges that skip layers.
 *   3. [[getXPositions]] – assign a horizontal pin column to every node I/O.
 *   4. [[routeGraph]]    – run the left-edge channel routing algorithm between
 *                          every adjacent pair of layers.
 */
object GraphRouter:

  /**
   * Partition the graph nodes into topological layers using Kahn's algorithm.
   *
   * Each layer contains the set of nodes whose input degree (after removing all
   * nodes from previous layers) is zero.  When `alignOutputs` is `true`, output
   * nodes are excluded from the zero-in-degree queue and are instead collected
   * into a final layer once all other nodes have been placed.
   *
   * @param graph        The [[Graph]] to sort.
   * @param alignOutputs When `true`, all output nodes are deferred to the last layer
   *                     so they are rendered at the same Z depth in the schematic.
   * @return An ordered [[kyo.Chunk]] of layers, each layer itself a [[kyo.Chunk]] of
   *         [[NodeId]]s.
   * @throws AssertionError if the graph contains a cycle that prevents a valid
   *                        topological ordering.
   */
  def getLayers(graph: Graph, alignOutputs: Boolean): Chunk[Chunk[NodeId]] =
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
          if inputDegree(output.id) == 0 && !(graph.isOutputNode(output.id) && alignOutputs) then nextZero += output.id

        inputDegree.remove(id)

      zeroInputDegree = nextZero.toSet

    if inputDegree.nonEmpty then
      if inputDegree.forall((id, _) => graph.isOutputNode(id)) && alignOutputs then
        layers += Chunk.from(inputDegree.keys)
      else
        throw new AssertionError("Graph contains a cycle")

    Chunk.from(layers)

  /**
   * Insert [[NodeType.Relay]] nodes wherever an edge skips more than one layer.
   *
   * The left-edge channel algorithm only routes wires between adjacent layers.
   * When a node in layer `y` has an output to a node in layer `y + k` (k > 1)
   * a chain of `k - 1` relay nodes is inserted at layers `y+1 … y+k-1` so that
   * every edge spans exactly one layer boundary.
   *
   * @param graph  The original logic graph.
   * @param layers The layered partition produced by [[getLayers]].
   * @return A pair of the updated graph (with relay nodes added) and the updated
   *         layer assignment (with relay nodes placed in the correct layers).
   */
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

  /**
   * Assign a horizontal pin column ([[PinX]]) to every `(NodeId, inputIndex)` pair
   * in the layered graph.
   *
   * Within each layer, nodes are placed left-to-right in order of appearance.
   * Multi-input nodes (OR, AND, XOR) occupy consecutive columns equal to their
   * [[NodeType.sizeX]].
   *
   * @param graph  The graph whose positions are to be computed.
   * @param layers The layer partition produced by [[getLayers]] / [[addRelays]].
   * @return A map from [[NodeOutput]] (node + input index) to its [[PinX]] column.
   */
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

  /**
   * Build the initial (track-free) [[Channel]] between two adjacent layers.
   *
   * For every node in the `from` layer a [[Net]] is created whose left endpoint
   * is the node's output column and whose right endpoint is the column of the
   * target input in the `to` layer.
   *
   * @see [[https://github.com/itsfrank/MinecraftHDL]]
   *
   * @param graph  The logic graph.
   * @param xPos   The column assignments produced by [[getXPositions]].
   * @param from   The source layer (nodes driving the channel).
   * @param to     The destination layer (nodes receiving from the channel).
   * @return An un-routed [[Channel]] containing all nets but no track assignments.
   */
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

  /**
   * Determine whether routing the given net would create a cycle in the channel.
   *
   * A cycle exists when following the chain of nets starting at `at.end`
   * eventually reaches `at.start` without leaving the channel.
   *
   * @param channel The channel to inspect.
   * @param done    The set of nets that have already been fully routed (cycle-breaking
   *                stops at these).
   * @param at      The net being evaluated.
   * @return `true` if placing `at` as-is would create a cycle.
   */
  def hasCycleAt(channel: Channel, done: Set[NetId], at: Net): Boolean =
    def rec(current: PinX): Boolean =
      channel.getNetAt(current).exists((id, net) =>
        if net.start == net.end || done.contains(id) then false
        else if current == at.start then true
        else rec(net.end)
      )

    rec(at.end)

  /**
   * Break the cycle for the net at `netId` if one exists, by rerouting to an outer
   * column.
   *
   * If [[hasCycleAt]] returns `true` for the net, [[Channel.reroute]] is called to
   * move the conflicting portion of the net outside the current column range.
   * The net is always added to the `done` set regardless.
   *
   * @param channel The current channel state.
   * @param done    Previously processed nets.
   * @param netId   The net to evaluate and potentially reroute.
   * @return A pair of the (possibly updated) channel and the updated `done` set.
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

  /**
   * Assign the net at `netId` to a track in the channel using the left-edge algorithm.
   *
   * The algorithm searches for the first existing track whose rightmost net ends before
   * the left endpoint of the new net (and satisfies additional constraints when
   * `optimize` is enabled).  If no such track is found a new track is created.
   *
   * @param optimize Whether the optimised track-sharing heuristic is enabled.
   * @param channel  The channel whose tracks are to be updated.
   * @param netId    The net to assign.
   * @return The updated [[Channel]] with the net placed in a track.
   */
  def assignTrack(optimize: Boolean)(channel: Channel, netId: NetId): Channel =
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

        (channel.getTrackEnd(track) < net.left || (hasSameStart && optimize)) && !isTrackBeforeDest
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

  /**
   * Produce a routing order for the inner nets of a channel.
   *
   * Nets are ordered so that a net is scheduled only after all nets whose
   * right endpoint coincides with its left endpoint have been placed (a
   * dependency-aware topological sort).  Outer-column nets are excluded and
   * will be appended afterwards.
   *
   * @param channel The channel whose inner nets are to be sorted.
   * @return A [[kyo.Chunk]] of [[NetId]]s in the order they should be assigned to tracks.
   */
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

  /**
   * Fully route a [[Channel]]: break cycles, sort nets, and assign tracks.
   *
   * @param channel  The un-routed channel (nets present, tracks empty).
   * @param optimize Whether the optimised track-sharing heuristic is enabled.
   * @return The routed [[Channel]] with all nets assigned to tracks.
   */
  def routeChannel(channel: Channel, optimize: Boolean): Channel =
    val (withoutCycle, _) = Chunk
      .range(NetId(0), NetId.assume(channel.nets.size))
      .foldLeft((channel, Set.empty[NetId])):
        case ((channel, done), id) =>
          breakCycle(channel, done, id)

    val sortedNets = sortNets(withoutCycle)

    val sortedNetsThenOuters = sortedNets ++ Chunk.range(NetId.assume(sortedNets.size), NetId.assume(withoutCycle.nets.size))
    sortedNetsThenOuters.foldLeft(withoutCycle)(assignTrack(optimize))

  /**
   * Route the entire graph: create and route a [[Channel]] between every pair of
   * adjacent layers.
   *
   * @see [[https://rtldigitaldesign.blogspot.com/2019/07/left-edge-channel-algorithm-for.html]]
   *
   * @param graph        The logic graph (with relays already inserted).
   * @param layers       The layered partition of the graph.
   * @param optimize     Whether the optimised routing heuristic is enabled.
   * @return One [[Channel]] per inter-layer boundary, in layer order.
   */
  // https://rtldigitaldesign.blogspot.com/2019/07/left-edge-channel-algorithm-for.html
  @nowarn("msg=exhaustive")
  def routeGraph(graph: Graph, layers: Chunk[Chunk[NodeId]], optimize: Boolean): Chunk[Channel] =
    val xPos = getXPositions(graph, layers)
    Chunk.from(
      layers.sliding(2).map:
        case Chunk(from, to) =>
          val channel = createChannel(graph, xPos, from, to)
          routeChannel(channel, optimize)
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
