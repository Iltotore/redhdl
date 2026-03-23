package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.graph.Channel
import io.github.iltotore.redhdl.graph.Graph
import io.github.iltotore.redhdl.graph.Net
import io.github.iltotore.redhdl.graph.NetId
import io.github.iltotore.redhdl.graph.NodeId
import io.github.iltotore.redhdl.graph.NodeType
import io.github.iltotore.redhdl.graph.PinX
import io.github.iltotore.redhdl.graph.TrackId
import java.nio.file.Files
import java.nio.file.Path as JPath
import kyo.*
import scala.util.Using

/**
 * Translates a routed logic [[Graph]] into a Minecraft [[Structure]] (schematic).
 *
 * The generator works layer by layer:
 *   1. Each layer is rendered as a row of gate schematics placed at the correct
 *      X/Z offsets ([[putLayer]]).
 *   2. Each inter-layer [[Channel]] is rendered as a set of redstone wire bridges
 *      ([[putChannel]] / [[putNet]]).
 *
 * Wires are coloured with blocks taken from the ambient [[SchematicContext]] palette,
 * cycling by the horizontal pin column index.  Repeaters are inserted at fixed
 * intervals to maintain signal strength over long runs.
 */
object SchematicGenerator:

  /** Z depth of a single gate schematic, in blocks. */
  private val gateSizeZ: Int = 4

  /** Total Z space consumed by one layer (gate + entry/exit wire rows). */
  private val layerSizeZ: Int = gateSizeZ + 2

  /** Horizontal spacing between consecutive pin columns, in blocks. */
  private val columnSpacing: Int = 2

  /** Vertical row spacing between consecutive channel tracks, in blocks. */
  private val trackSpacing: Int = 4

  extension (structure: Structure)

    /**
     * Draw a horizontal (X-axis) circuit wire from `position` to `to`, placing wool
     * blocks on the lower layer and redstone wire on top, and inserting repeaters at
     * regular intervals to maintain signal strength.
     *
     * @param position The starting [[BlockPos]] of the wire.
     * @param to       The destination X coordinate.
     * @param color    The block used as the solid wire substrate.
     * @param delay    The repeater delay for inserted repeaters.
     * @return The updated [[Structure]] with the wire placed.
     */
    def withCircuitLineX(position: BlockPos, to: Int, color: Block, delay: RepeaterDelay): Structure =
      val base = structure
        .withLineX(position, to, color)
        .withLineX(position + (0, 1, 0), to, Block.RedstoneWire)

      val (repeaterPositions, repeaterFacing) =
        if position.x < to then ((position.x + 13) to to by 14, Block.Facing.West)
        else ((position.x - 13) to to by -14, Block.Facing.East)

      repeaterPositions.foldLeft(base)((acc, x) =>
        acc.withBlock(
          position = BlockPos(x, position.y + 1, position.z),
          block = Block.Repeater(delay, repeaterFacing),
          overrideBlock = true
        )
      )

    /**
     * Draw a vertical (Z-axis) circuit wire from `position` to `to`, placing wool
     * blocks on the lower layer and redstone wire on top, and inserting repeaters at
     * regular intervals to maintain signal strength.
     *
     * @param position The starting [[BlockPos]] of the wire.
     * @param to       The destination Z coordinate.
     * @param color    The block used as the solid wire substrate.
     * @param delay    The repeater delay for inserted repeaters.
     * @return The updated [[Structure]] with the wire placed.
     */
    def withCircuitLineZ(position: BlockPos, to: Int, color: Block, delay: RepeaterDelay): Structure =
      val base = structure
        .withLineZ(position, to, color)
        .withLineZ(position + (0, 1, 0), to, Block.RedstoneWire)

      val (repeaterPositions, repeaterFacing) =
        if position.z < to then ((position.z + 15) to to by 15, Block.Facing.North)
        else ((position.z - 15) to to by -15, Block.Facing.South)

      repeaterPositions.foldLeft(base)((acc, z) =>
        acc.withBlock(
          position = BlockPos(position.x, position.y + 1, z),
          block = Block.Repeater(delay, repeaterFacing),
          overrideBlock = true
        )
      )

  /**
   * Compute the Z height (in blocks) consumed by a routing channel.
   *
   * Only tracks that contain at least one non-single-line net contribute to the
   * height; tracks composed entirely of single-line (vertical) nets are collapsed.
   *
   * @param channel The [[Channel]] whose height is to be measured.
   * @return The total Z height of the channel in blocks, or `-1` if the channel is
   *         effectively empty.
   */
  def getChannelSize(channel: Channel): Int =
    channel.tracks.foldLeft(-1)((acc, track) =>
      if track.nets.forall(channel.getNet(_).isSingleLine) then acc
      else acc + trackSpacing
    )

  /**
   * Compute the bounding box required to hold the entire generated circuit.
   *
   * @param graph    The routed logic graph.
   * @param layers   The layer partition.
   * @param channels The routed channels between layers.
   * @return A [[BlockPos]] whose `x`, `y`, and `z` give the required schematic
   *         dimensions, within a [[SchematicGeneration]] computation.
   */
  def getNeededRegion(graph: Graph, layers: Chunk[Chunk[NodeId]], channels: Chunk[Channel]): BlockPos < SchematicGeneration = direct:
    val sizeX = channels.map(_.sizeX).max * columnSpacing
    val sizeY = 4
    val sizeZ = channels.map(getChannelSize).sum + layers.size * layerSizeZ

    BlockPos(sizeX, sizeY, sizeZ)

  /**
   * Paste a gate schematic into a target structure at the given position, optionally
   * recolouring it.
   *
   * When a `color` is provided, every white-wool block in the schematic is replaced
   * with that block before pasting (see [[Structure.recolor]]).
   *
   * @param tpe    The gate type whose schematic is to be pasted.
   * @param to     The target [[Structure]] to paste into.
   * @param at     The [[BlockPos]] offset at which to paste the schematic.
   * @param color  An optional replacement colour for white-wool wire blocks.
   * @return The updated structure within a [[SchematicGeneration]] computation.
   */
  def pasteGateSchematic(tpe: GateType, to: Structure, at: BlockPos, color: Option[Block] = None): Structure < SchematicGeneration =
    SchematicContext
      .getSchematic(tpe)
      .map: schem =>
        val used = color match
          case Some(c) => schem.recolor(c)
          case None    => schem
        to.withStructure(at, used)

  /**
   * Place a logic gate into a structure at the given position, adding a sign label
   * for input and output nodes.
   *
   * Input nodes get a sign above port 3 (the output face); output nodes get a sign
   * above port 0 (the input face).  All other gate types are pasted without a label.
   *
   * @param tpe       The [[NodeType]] determining the gate schematic and labelling.
   * @param structure The target [[Structure]].
   * @param at        The [[BlockPos]] origin for the gate.
   * @param color     An optional colour override for white-wool wire blocks.
   * @return The updated structure within a [[SchematicGeneration]] computation.
   */
  def putGate(tpe: NodeType, structure: Structure, at: BlockPos, color: Option[Block]): Structure < SchematicGeneration = direct:
    tpe match
      case NodeType.Input(name) =>
        pasteGateSchematic(tpe.toGateType, structure, at, color)
          .now
          .withBlock(at + (0, 2, 3), Block.Sign(Block.Rotation.North, name.value))
      case NodeType.Output(name) =>
        pasteGateSchematic(tpe.toGateType, structure, at, color)
          .now
          .withBlock(at + (0, 2, 0), Block.Sign(Block.Rotation.South, name.value))
      case _ => pasteGateSchematic(tpe.toGateType, structure, at, color).now

  /**
   * Determine the Z offset of a net's track within a channel.
   *
   * Iterates over the channel's tracks and accumulates Z offsets, skipping tracks
   * composed entirely of single-line nets.
   *
   * @param channel The channel containing the net.
   * @param id      The [[NetId]] whose track Z position is requested.
   * @return The Z offset (in blocks) of the track row that owns `id`.
   * @throws AssertionError if `id` is not found in any track.
   */
  def getTrackZ(channel: Channel, id: NetId): Int =
    Loop(channel.tracks, 1):
      case (track +: remaining, spacing) =>
        val additionalSpacing =
          if track.nets.forall(channel.getNet(_).isSingleLine) then 0
          else trackSpacing

        if track.nets.contains(id) then Loop.done(spacing)
        else
          Loop.continue(remaining, spacing + additionalSpacing)

      case _ => throw AssertionError(s"Not track for net $id")
    .eval

  /**
   * Place the redstone wiring for a single net into the structure.
   *
   * A single-line net (same start and end X) is drawn as a pure vertical Z-axis wire.
   * A crossing net is drawn with a vertical approach segment, a horizontal bridge at
   * the track's Z height, and a vertical departure segment.  Rerouted outer nets are
   * handled recursively via [[Net.outerNet]].
   *
   * @param channel  The channel that contains this net.
   * @param id       The [[NetId]] of the net to place.
   * @param net      The [[Net]] data for `id`.
   * @param structure The target structure.
   * @param at       The origin [[BlockPos]] of the channel region.
   * @param startZ   The Z coordinate within the channel at which vertical wires begin.
   * @param color    The block to use for wire segments.
   * @return The updated structure within a [[SchematicGeneration]] computation.
   */
  def putNet(channel: Channel, id: NetId, net: Net, structure: Structure, at: BlockPos, startZ: Int, color: Block): Structure < SchematicGeneration =
    SchematicContext.getRepeaterDelay.map(delay =>
      val trackZ = getTrackZ(channel, id)

      val endZ = getChannelSize(channel) - 1
      val startX = net.start.value * columnSpacing
      val endX = net.end.value * columnSpacing

      if startX == endX then
        structure.withCircuitLineZ(at + (startX, 0, startZ), at.z + endZ, color, delay)
      else
        val withoutLineAfterBridge =
          structure
            // Line before bridge
            .withCircuitLineZ(at + (startX, 0, startZ), at.z + math.max(0, trackZ - 3), color, delay)
            // Bridge
            .withCircuitLineX(at + (startX, 2, trackZ), at.x + endX, color, delay)
            // Bridge start repeater
            .withBlock(at + (startX, 0, trackZ - 2), color)
            .withBlock(at + (startX, 1, trackZ - 2), Block.Repeater(delay), overrideBlock = true)
            // Bridge end repeater
            .withBlock(at + (endX, 0, trackZ + 2), color)
            .withBlock(at + (endX, 1, trackZ + 2), Block.Repeater(delay), overrideBlock = true)
            // Bridge start
            .withBlock(at + (startX, 1, trackZ - 1), color, overrideBlock = true)
            .withBlock(at + (startX, 2, trackZ - 1), Block.RedstoneWire)
            // and end
            .withBlock(at + (endX, 1, trackZ + 1), color, overrideBlock = true)
            .withBlock(at + (endX, 2, trackZ + 1), Block.RedstoneWire)

        net.outerNet match
          case Absent =>
            withoutLineAfterBridge
              .withCircuitLineZ(at + (endX, 0, math.min(endZ, trackZ + 3)), at.z + endZ, color, delay)
          case Present(outerId) =>
            val outerNet = channel.getNet(outerId)
            putNet(channel, outerId, outerNet, withoutLineAfterBridge, at, trackZ + 3, color)
    )

  /**
   * Place all gate nodes of one layer into the structure.
   *
   * Gates are placed left-to-right in pin-column order.  Wire entry repeaters (on
   * the input face of the gate) and exit repeaters (on the output face) are inserted
   * for every non-input, non-output gate.
   *
   * @param layer     The sequence of [[NodeType]]s to place, in left-to-right order.
   * @param structure The target structure.
   * @param at        The origin [[BlockPos]] for this layer row.
   * @param incoming  The channel feeding signals into this layer (used for colouring).
   * @param outgoing  The channel receiving signals from this layer (used for colouring).
   * @return The updated structure within a [[SchematicGeneration]] computation.
   */
  def putLayer(
      layer: Chunk[NodeType],
      structure: Structure,
      at: BlockPos,
      incoming: Option[Channel],
      outgoing: Option[Channel]
  ): Structure < SchematicGeneration =
    val layerSize = layer.map(_.sizeX).sum

    Loop(structure, layer, 0):
      case (struct, nodeType +: tail, x) =>
        for
          color <- SchematicContext.getPaletteBlock(PinX.assume(x))
          delay <- SchematicContext.getRepeaterDelay
          withInputs =
            if nodeType.isInput then struct
            else
              Range(0, nodeType.sizeX).foldLeft(struct)((s, pin) =>
                val realX = (x + pin) * columnSpacing
                s
                  .withBlock(at + (realX, 0, 0), color, overrideBlock = true)
                  .withBlock(at + (realX, 1, 0), Block.Repeater(delay))
              )

          withIO =
            if nodeType.isOutput then withInputs
            else
              withInputs
                .withBlock(at + (x * columnSpacing, 0, gateSizeZ + 1), color)
                .withBlock(at + (x * columnSpacing, 1, gateSizeZ + 1), Block.Repeater(delay))

          updated <- putGate(
            nodeType,
            withIO,
            at + (x * columnSpacing, 0, 1),
            Some(color)
          )
        yield Loop.continue(updated, tail, x + nodeType.sizeX)

      case (struct, _, _) => Loop.done(struct)

  /**
   * Place all nets of a routing channel into the structure.
   *
   * Nets that start in an outer column (used for cycle-breaking) are skipped;
   * they are handled as part of the inner net that references them via
   * [[Net.outerNet]].
   *
   * @param channel   The [[Channel]] to render.
   * @param structure The target structure.
   * @param at        The origin [[BlockPos]] for the channel region.
   * @return The updated structure within a [[SchematicGeneration]] computation.
   */
  def putChannel(channel: Channel, structure: Structure, at: BlockPos): Structure < SchematicGeneration =
    Kyo.foldLeft(channel.nets.zipWithIndex)(structure):
      case (struct, (net, id)) =>
        if channel.isOuterColumn(net.start) then struct
        else
          SchematicContext.getPaletteBlock(net.start).map(
            putNet(channel, NetId.assume(id), net, struct, at, 0, _)
          )

  /**
   * Generate the complete [[Structure]] for a routed graph.
   *
   * The method places the first layer, then iterates over the remaining layers
   * interleaved with their incoming channels, accumulating gate placements and
   * wire placements into a single flat structure.
   *
   * @param graph    The fully routed logic graph.
   * @param layers   The layered partition of the graph.
   * @param channels One [[Channel]] per inter-layer boundary.
   * @return The complete [[Structure]] within a [[SchematicGeneration]] computation.
   */
  def generateStructure(graph: Graph, layers: Chunk[Chunk[NodeId]], channels: Chunk[Channel]): Structure < SchematicGeneration = direct:
    val emptyStructure = Structure.empty(getNeededRegion(graph, layers, channels).now)
    val firstOutgoing = channels.headOption
    val withFirstLayer = putLayer(layers(0).map(id => graph.getNode(id).tpe), emptyStructure, BlockPos(0, 0, 0), None, firstOutgoing).now

    Loop(withFirstLayer, layers.tail, channels, layerSizeZ):
      case (struct, layer +: remainingLayers, channel +: remainingChannels, z) =>
        direct:
          val channelSize = getChannelSize(channel)
          val layerStart = z + channelSize

          val withChannel =
            if channelSize <= 0 then struct
            else putChannel(channel, struct, BlockPos(0, 0, z)).now

          val nextOutgoing = remainingChannels.drop(1).headOption

          val withChannelAndLayer =
            putLayer(layer.map(id => graph.getNode(id).tpe), withChannel, BlockPos(0, 0, layerStart), Some(channel), nextOutgoing).now
          Loop.continue[Structure, Chunk[Chunk[NodeId.T]], Chunk[Channel], Int, Structure](
            withChannelAndLayer,
            remainingLayers,
            remainingChannels,
            layerStart + layerSizeZ
          )

      case (struct, _, _, _) => Loop.done(struct)
    .now
