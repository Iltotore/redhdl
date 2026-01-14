package io.github.iltotore.redhdl.minecraft

import kyo.Chunk
import io.github.iltotore.redhdl.graph.NodeId
import io.github.iltotore.redhdl.graph.Graph
import io.github.iltotore.redhdl.graph.Channel
import com.sk89q.worldedit.extent.clipboard.Clipboard
import com.sk89q.worldedit.math.BlockVector3
import com.sk89q.worldedit.regions.CuboidRegion
import com.sk89q.worldedit.regions.Region

object SchematicGeneration:

  private val gateSizeZ: Int = 6

  def getRegion(graph: Graph, layers: Chunk[Chunk[NodeId]], channels: Chunk[Channel]): Region =
    val sizeX = channels.map(_.sizeX).max * 2
    val sizeY = 4
    val sizeZ = channels.map(_.sizeZ).sum * + (layers.size - 1) * gateSizeZ + 2

    CuboidRegion(BlockVector3(0, 0, 0), BlockVector3(sizeX, sizeY, sizeZ))

  def generateStructure(graph: Graph, layers: Chunk[Chunk[NodeId]], channels: Chunk[Channel]): Clipboard = ???