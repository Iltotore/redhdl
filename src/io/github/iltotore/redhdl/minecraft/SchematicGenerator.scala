package io.github.iltotore.redhdl.minecraft

import com.sk89q.worldedit.extent.clipboard.BlockArrayClipboard
import com.sk89q.worldedit.extent.clipboard.Clipboard
import com.sk89q.worldedit.function.operation.Operations
import com.sk89q.worldedit.math.BlockVector3
import com.sk89q.worldedit.regions.CuboidRegion
import com.sk89q.worldedit.regions.Region
import com.sk89q.worldedit.world.block.BlockTypes
import io.github.iltotore.redhdl.graph.Channel
import io.github.iltotore.redhdl.graph.Graph
import io.github.iltotore.redhdl.graph.NodeId
import io.github.iltotore.redhdl.graph.NodeType
import kyo.*
import org.enginehub.linbus.tree.LinCompoundTag
import org.enginehub.linbus.tree.LinListTag
import org.enginehub.linbus.tree.LinStringTag
import org.enginehub.linbus.tree.LinTagType
import com.sk89q.worldedit.world.block.BaseBlock
import io.github.iltotore.redhdl.graph.Net
import io.github.iltotore.redhdl.graph.NetId
import scala.util.Using
import com.sk89q.worldedit.WorldEdit
import com.sk89q.worldedit.session.ClipboardHolder
import com.sk89q.worldedit.extent.clipboard.io.ClipboardFormats
import java.nio.file.Path as JPath
import java.nio.file.Files

object SchematicGenerator:

  private val gateSizeZ: Int = 4
  private val layerSizeZ: Int = gateSizeZ + 2
  private val columnSpacing: Int = 2
  private val trackSpacing: Int = 4

  def getNeededRegion(graph: Graph, layers: Chunk[Chunk[NodeId]], channels: Chunk[Channel]): Region < SchematicGeneration = direct:
    val sizeX = channels.map(_.sizeX).max * 2
    val sizeY = 4
    val sizeZ = channels.map(_.sizeZ).sum * +(layers.size - 1) * layerSizeZ + 2

    CuboidRegion(BlockVector3(0, 0, 0), BlockVector3(sizeX, sizeY, sizeZ))

  def pasteSchematic(clipboard: Clipboard, to: Clipboard, at: BlockVector3): Unit =
    val operation = ClipboardHolder(clipboard)
      .createPaste(to)
      .to(at)
      .build()

    Operations.complete(operation)

  def pasteGateSchematic(tpe: GateType, to: Clipboard, at: BlockVector3): Unit < SchematicGeneration =
    SchematicContext
      .getSchematic(tpe)
      .map(pasteSchematic(_, to, at))

  def createSign(title: String): BaseBlock =
    val messages = LinListTag.of(
      LinTagType.stringTag,
      java.util.Arrays.asList(
        LinStringTag.of(s"""{"text":"$title"}"""),
        LinStringTag.of("""{"text":""}"""),
        LinStringTag.of("""{"text":""}"""),
        LinStringTag.of("""{"text":""}""")
      )
    )

    val frontText = LinCompoundTag.of(java.util.Map.of(
      "messages",
      messages
    ))

    val nbt = LinCompoundTag.of(java.util.Map.of("front_text", frontText))
    BlockTypes.SIGN.getDefaultState().toBaseBlock(nbt)

  def putGate(tpe: NodeType, clipboard: Clipboard, at: BlockVector3): Unit < SchematicGeneration = direct:
    tpe match
      case NodeType.Input(name) =>
        pasteGateSchematic(tpe.toGateType, clipboard, at).now
        clipboard.setBlock(at.add(0, 1, 0), createSign(name.value))
      case NodeType.Output(name) =>
        pasteGateSchematic(tpe.toGateType, clipboard, at).now
        clipboard.setBlock(at.add(0, 1, 0), createSign(name.value))
      case _ => pasteGateSchematic(tpe.toGateType, clipboard, at).now

  def putNet(channel: Channel, id: NetId, net: Net, clipboard: Clipboard, at: BlockVector3): Unit < SchematicGeneration = direct:
    val trackId = channel.getNetTrack(id).get
    val trackZ = trackId.value * trackSpacing + 1
    val endZ = channel.tracks.size * (trackSpacing + 1) + 1
    val startX = net.start.value * columnSpacing
    val endX = net.end.value * columnSpacing

    for z <- 0 until trackZ - 2 do
      clipboard.setBlock(at.add(startX, 0, z), BlockTypes.WHITE_WOOL.getDefaultState().toBaseBlock())
      clipboard.setBlock(at.add(startX, 1, z), BlockTypes.REDSTONE_WIRE.getDefaultState().toBaseBlock())

    for z <- trackZ + 3 until endZ do
      clipboard.setBlock(at.add(endX, 0, z), BlockTypes.WHITE_WOOL.getDefaultState().toBaseBlock())
      clipboard.setBlock(at.add(endX, 1, z), BlockTypes.REDSTONE_WIRE.getDefaultState().toBaseBlock())

    // Bridge start repeater
    clipboard.setBlock(at.add(startX, 0, trackZ - 2), BlockTypes.WHITE_WOOL.getDefaultState().toBaseBlock())
    clipboard.setBlock(at.add(startX, 1, trackZ - 2), BlockTypes.REPEATER.getDefaultState().toBaseBlock())
    // Bridge end repeater
    clipboard.setBlock(at.add(endX, 0, trackZ + 2), BlockTypes.WHITE_WOOL.getDefaultState().toBaseBlock())
    clipboard.setBlock(at.add(endX, 1, trackZ + 2), BlockTypes.REPEATER.getDefaultState().toBaseBlock())
    // Bridge start
    clipboard.setBlock(at.add(startX, 1, trackZ - 1), BlockTypes.WHITE_WOOL.getDefaultState().toBaseBlock())
    clipboard.setBlock(at.add(startX, 2, trackZ - 1), BlockTypes.REDSTONE_WIRE.getDefaultState().toBaseBlock())
    // and end
    clipboard.setBlock(at.add(endX, 1, trackZ + 1), BlockTypes.WHITE_WOOL.getDefaultState().toBaseBlock())
    clipboard.setBlock(at.add(endX, 2, trackZ + 1), BlockTypes.REDSTONE_WIRE.getDefaultState().toBaseBlock())

    // The bridge itself
    for x <- startX until endX do
      clipboard.setBlock(at.add(x, 1, trackZ), BlockTypes.WHITE_WOOL.getDefaultState().toBaseBlock())
      clipboard.setBlock(at.add(x, 2, trackZ), BlockTypes.REDSTONE_WIRE.getDefaultState().toBaseBlock())    

  def putLayer(layer: Chunk[NodeType], clipboard: Clipboard, at: BlockVector3): Unit < SchematicGeneration = direct:
    val layerSize = layer.map(_.sizeX).sum
    
    for x <- 0 to layerSize do
      clipboard.setBlock(at.add(x * columnSpacing, 0, 0), BlockTypes.WHITE_WOOL.getDefaultState().toBaseBlock())
      clipboard.setBlock(at.add(x * columnSpacing, 1, 0), BlockTypes.REPEATER.getDefaultState().toBaseBlock())
    
    for x <- 0 to layerSize do
      clipboard.setBlock(at.add(x * columnSpacing, 0, gateSizeZ + 1), BlockTypes.WHITE_WOOL.getDefaultState().toBaseBlock())
      clipboard.setBlock(at.add(x * columnSpacing, 1, gateSizeZ + 1), BlockTypes.REPEATER.getDefaultState().toBaseBlock())

    Loop(layer, 0):
      case (nodeType +: tail, x) =>
        val sizeX = nodeType.sizeX
        putGate(nodeType, clipboard, at.add(x * columnSpacing, 0, 1))
          .andThen(Loop.continue(tail, x + sizeX))
    
      case (_, x) => Loop.done(())
    .now

  def putChannel(channel: Channel, clipboard: Clipboard, at: BlockVector3): Unit < SchematicGeneration =
    Kyo.foreachDiscard(channel.nets.zipWithIndex)((net, id) =>
      putNet(channel, NetId.assume(id), net, clipboard, at)
    )

  def generateStructure(graph: Graph, layers: Chunk[Chunk[NodeId]], channels: Chunk[Channel]): Clipboard < SchematicGeneration = direct:
    println("TEST")
    
    val clipboard = BlockArrayClipboard(getNeededRegion(graph, layers, channels).now)

    println(s"Region: ${clipboard.getRegion}")

    putLayer(layers(0).map(id => graph.getNode(id).tpe), clipboard, BlockVector3.at(0, 0, 0)).now

    println(s"Layer 0")

    Loop(layers.tail, channels, layerSizeZ):
      case (layer +: remainingLayers, channel +: remainingChannels, z) =>
        direct:
          putLayer(layer.map(id => graph.getNode(id).tpe), clipboard, BlockVector3.at(0, 0, z)).now
          putChannel(channel, clipboard, BlockVector3.at(0, 0, z + layerSizeZ)).now
          Loop.continue(
            remainingLayers,
            remainingChannels,
            z + layerSizeZ + channel.tracks.size * (trackSpacing + 1) + 1
          )

      case _ => Loop.done(())
    .now

    clipboard

  def saveSchematic(clipboard: Clipboard, path: JPath): Unit < (SchematicGeneration & Sync) =
    val format = ClipboardFormats.findByFile(path.toFile)
    
    if format == null then Abort.fail(SchematicFailure.InvalidSchematic(path.toString, ""))
    else
      Using.resource(Files.newOutputStream(path))(output =>
        format.getWriter(output).write(clipboard)
      )

  def generateAndSaveStructure(
    graph: Graph,
    layers: Chunk[Chunk[NodeId]],
    channels: Chunk[Channel],
    path: JPath
  ): Unit < (SchematicGeneration & Sync) =
    println("hey")
    // generateStructure(graph, layers, channels)
    //   .map(r => {
    //     println("SAVING")
    //     r
    //   })
    //   .map(saveSchematic(_, path))
