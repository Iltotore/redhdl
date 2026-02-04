package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.graph.Channel
import io.github.iltotore.redhdl.graph.Graph
import io.github.iltotore.redhdl.graph.NodeId
import io.github.iltotore.redhdl.graph.NodeType
import kyo.*
import io.github.iltotore.redhdl.graph.Net
import io.github.iltotore.redhdl.graph.NetId
import scala.util.Using
import java.nio.file.Path as JPath
import java.nio.file.Files
import io.github.ensgijs.nbt.io.BinaryNbtHelpers
import io.github.ensgijs.nbt.io.CompressionType
import io.github.iltotore.redhdl.graph.Channel

object SchematicGenerator:

  private val gateSizeZ: Int = 4
  private val layerSizeZ: Int = gateSizeZ + 2
  private val columnSpacing: Int = 2
  private val trackSpacing: Int = 4

  def getChannelSize(channel: Channel): Int =
    channel.tracks.size * trackSpacing + 4

  def getNeededRegion(graph: Graph, layers: Chunk[Chunk[NodeId]], channels: Chunk[Channel]): BlockPos < SchematicGeneration = direct:
    val sizeX = channels.map(_.sizeX).max * columnSpacing
    val sizeY = 4
    val sizeZ = channels.map(getChannelSize).sum + layers.size * layerSizeZ

    BlockPos(sizeX, sizeY, sizeZ)

  def pasteGateSchematic(tpe: GateType, to: Structure, at: BlockPos): Structure < SchematicGeneration =
    SchematicContext
      .getSchematic(tpe)
      .map(to.withStructure(at, _))

  def putGate(tpe: NodeType, structure: Structure, at: BlockPos): Structure < SchematicGeneration = direct:
    println(s"Put gate $tpe at $at")
    tpe match
      case NodeType.Input(name) =>
        pasteGateSchematic(tpe.toGateType, structure.withBlock(at, Block("minecraft:cyan_wool")), at)
          .now
          .withBlock(at + (0, 2, 3), Block.Sign(name.value))
      case NodeType.Output(name) =>
        pasteGateSchematic(tpe.toGateType, structure.withBlock(at, Block("minecraft:blue_wool")), at)
          .now
          .withBlock(at + (0, 2, 0), Block.Sign(name.value))
      case _ => pasteGateSchematic(tpe.toGateType, structure.withBlock(at, Block("minecraft:blue_wool")), at).now

  def putNet(channel: Channel, id: NetId, net: Net, structure: Structure, at: BlockPos, startZ: Int): Structure < SchematicGeneration = direct:
    println(s"Put net $id at $at")
    
    val trackId = channel.getNetTrack(id).get
    val trackZ = trackId.value * trackSpacing + 3
    val endZ = getChannelSize(channel)
    val startX = net.start.value * columnSpacing
    val endX = net.end.value * columnSpacing

    val withoutLineAfterBridge =
      structure
        .withBlock(at, Block("minecraft:red_wool"))
        // Line before bridge
        .withLineZ(at + (startX, 0, startZ), at.z + trackZ - 3, Block("minecraft:pink_wool"))
        .withLineZ(at + (startX, 1, startZ), at.z + trackZ - 3, Block("minecraft:redstone_wire"))
        // Bridge
        .withLineX(at + (startX, 2, trackZ), at.x + endX, Block("minecraft:lime_wool"))
        .withLineX(at + (startX, 3, trackZ), at.x + endX, Block("minecraft:redstone_wire"))
        // Bridge start repeater
        .withBlock(at + (startX, 0, trackZ - 2), Block("minecraft:orange_wool"))
        .withBlock(at + (startX, 1, trackZ - 2), Block("minecraft:repeater"), overrideBlock = true)
        // Bridge end repeater
        .withBlock(at + (endX, 0, trackZ + 2), Block("minecraft:green_wool"))
        .withBlock(at + (endX, 1, trackZ + 2), Block("minecraft:repeater"), overrideBlock = true)
        // Bridge start
        .withBlock(at + (startX, 1, trackZ - 1), Block("minecraft:magenta_wool"), overrideBlock = true)
        .withBlock(at + (startX, 2, trackZ - 1), Block("minecraft:redstone_wire"))
        // and end
        .withBlock(at + (endX, 1, trackZ + 1), Block("minecraft:black_wool"), overrideBlock = true)
        .withBlock(at + (endX, 2, trackZ + 1), Block("minecraft:redstone_wire"))

    val withLineAfterBridge =
      if channel.isOuterColumn(net.start) then withoutLineAfterBridge
      else
        withoutLineAfterBridge
          .withLineZ(at + (endX, 0, trackZ + 3), at.z + endZ, Block("minecraft:purple_wool"))
          .withLineZ(at + (endX, 1, trackZ + 3), at.z + endZ, Block("minecraft:redstone_wire"))

    net.outerNet match
      case Absent => withLineAfterBridge
      case Present(outerId) =>
        val outerNet = channel.getNet(outerId)
        putNet(channel, outerId, outerNet, withLineAfterBridge, at, trackZ + 3).now

  def putLayer(layer: Chunk[NodeType], structure: Structure, at: BlockPos): Structure < SchematicGeneration = direct:
    val layerSize = layer.map(_.sizeX).sum
    println(s"Layer at: $at, size: $layerSize")

    Loop(structure, layer, 0):
      case (struct, nodeType +: tail, x) =>
        val sizeX = nodeType.sizeX

        val withIO = Range(0, sizeX).foldLeft(struct)((s, pin) =>
          val realX = (x + pin) * columnSpacing
          println(s"Layer place at X=$x (true pos: $realX)")

          s
            .withBlock(at + (realX, 0, 0), Block("minecraft:yellow_wool"))  
            .withBlock(at + (realX, 1, 0), Block("minecraft:repeater"))  
        )
        .withBlock(at + (x * columnSpacing, 0, gateSizeZ + 1), Block("minecraft:yellow_wool"))
        .withBlock(at + (x * columnSpacing, 1, gateSizeZ + 1), Block("minecraft:repeater"))

        putGate(nodeType, withIO, at + (x * columnSpacing, 0, 1))
          .map(Loop.continue(_, tail, x + sizeX))
    
      case (struct, _, _) => Loop.done(struct)
    .now

  def putChannel(channel: Channel, structure: Structure, at: BlockPos): Structure < SchematicGeneration =
    Kyo.foldLeft(channel.nets.zipWithIndex)(structure):
      case (struct, (net, id)) =>
        if channel.isOuterColumn(net.start) then struct
        else putNet(channel, NetId.assume(id), net, struct, at, 0)
    
  def generateStructure(graph: Graph, layers: Chunk[Chunk[NodeId]], channels: Chunk[Channel]): Structure < SchematicGeneration = direct:
    println("TEST")
    
    val emptyStructure = Structure.empty(getNeededRegion(graph, layers, channels).now)
    val withFirstLayer = putLayer(layers(0).map(id => graph.getNode(id).tpe), emptyStructure, BlockPos(0, 0, 0)).now

    println(s"Layer 0, sizes: ${layers.tail.size}, ${channels.size}")

    Loop(withFirstLayer, layers.tail, channels, layerSizeZ):
      case (struct, layer +: remainingLayers, channel +: remainingChannels, z) =>
        direct:
          val layerStart = z + getChannelSize(channel) + 1

          println(s"Channel size Z: ${getChannelSize(channel)}")

          val withChannel = putChannel(channel, struct, BlockPos(0, 0, z)).now
          val withChannelAndLayer = putLayer(layer.map(id => graph.getNode(id).tpe), withChannel, BlockPos(0, 0, layerStart)).now
          Loop.continue[Structure, Chunk[Chunk[NodeId.T]], Chunk[Channel], Int, Structure](
            withChannelAndLayer,
            remainingLayers,
            remainingChannels,
            layerStart + layerSizeZ
          )

      case (struct, _, _, _) => Loop.done(struct)
    .now

  def saveSchematic(structure: Structure, path: String): Unit < (SchematicGeneration & Sync) =
    BinaryNbtHelpers.write(Structure.saveSponge(structure), path, CompressionType.GZIP): Unit

  def generateAndSaveStructure(
    graph: Graph,
    layers: Chunk[Chunk[NodeId]],
    channels: Chunk[Channel],
    path: String
  ): Unit < (SchematicGeneration & Sync) =
    generateStructure(graph, layers, channels)
      .map(saveSchematic(_, path))
