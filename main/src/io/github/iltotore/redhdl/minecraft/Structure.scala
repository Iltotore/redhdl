package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.minecraft.nbt.NBT
import java.io.IOException
import kyo.*
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.jdk.CollectionConverters.SetHasAsScala

case class Structure(dimensions: BlockPos, var blocks: Chunk[Block]):

  def width: Int = dimensions.x
  def height: Int = dimensions.y
  def length: Int = dimensions.z

  private def positionToIndex(position: BlockPos): Int =
    position.x + position.z * width + position.y * width * length

  private def indexToPosition(index: Int): BlockPos =
    val y = index / (width * length)
    val z = (index - y * width * length) / width
    val x = index - z * width - y * width * length

    BlockPos(x, y, z)

  def apply(position: BlockPos): Block = blocks(positionToIndex(position))

  // Returns true if the given position is inside the structure bounds
  private def inBounds(pos: BlockPos): Boolean =
    pos.x >= 0 && pos.y >= 0 && pos.z >= 0 &&
      pos.x < width && pos.y < height && pos.z < length

  def withBlock(position: BlockPos, block: Block, overrideBlock: Boolean = false): Structure =
    if !inBounds(position) then
      println(s"Warning: [${block.id}] out of bounds at ($position) (dims=$dimensions)")
      this
    else
      val existing = this(position)
      if overrideBlock || existing.id == "minecraft:air" then
        this.copy(blocks = blocks.updated(positionToIndex(position), block))
      else
        this

  def withLineX(positionA: BlockPos, to: Int, block: Block, overrideBlock: Boolean = false): Structure =
    val step = if to >= positionA.x then 1 else -1
    Range.inclusive(positionA.x, to, step).foldLeft(this)((structure, x) =>
      structure.withBlock(BlockPos(x, positionA.y, positionA.z), block, overrideBlock)
    )

  def withLineZ(positionA: BlockPos, to: Int, block: Block, overrideBlock: Boolean = false): Structure =
    val step = if to >= positionA.z then 1 else -1
    Range.inclusive(positionA.z, to, step).foldLeft(this)((structure, z) =>
      structure.withBlock(BlockPos(positionA.x, positionA.y, z), block, overrideBlock)
    )

  def withData(position: BlockPos, data: NBT.CompoundTag): Structure =
    this.withBlock(position, this(position).withData(data))

  def withBlockEntity(position: BlockPos, entity: BlockEntity): Structure =
    this.withBlock(position, this(position).copy(entity = Present(entity)))

  def withStructure(position: BlockPos, structure: Structure): Structure =
    val positions =
      for
        x <- 0 until structure.width
        y <- 0 until structure.height
        z <- 0 until structure.length
      yield BlockPos(x, y, z)

    positions.foldLeft(this)((result, pos) => result.withBlock(position + pos, structure(pos)))

  /**
   * Produce a copy of this structure where every white wool block is replaced
   * with the supplied colour.  This is used when pasting gate schematics so that
   * the graphic can be tinted according to the surrounding wire palette.
   */
  def recolor(color: Block): Structure =
    val newBlocks = blocks.map(b =>
      if b.id == "minecraft:white_wool" then color
      else b
    )
    this.copy(blocks = newBlocks)

  def toPaletteAndData: (Map[String, Int], Chunk[Byte]) =
    val (_, finalPalette, finalData) = blocks.foldLeft((0.toByte, Map.empty[String, Int], Chunk.empty[Byte])):
      case ((nextId, palette, data), block) =>
        val str = block.toSchemString
        if palette.contains(str) then (nextId, palette, data :+ palette(str).toByte)
        else ((nextId + 1).toByte, palette.updated(str, nextId), data :+ nextId)

    (finalPalette, finalData)

object Structure:

  def empty(dimensions: BlockPos, fill: Block = Block.Air): Structure =
    Structure(
      dimensions = dimensions,
      blocks = Chunk.fill(dimensions.x * dimensions.y * dimensions.z)(fill)
    )

  def loadSponge(path: String, root: NBT): Structure < Abort[SchematicFailure] = direct:
    val schematic = root.asCompound("Schematic").asCompound
    val version = schematic("Version").asInt
    if version < 3 then
      Abort.fail(SchematicFailure.InvalidSchematic(path, s"Only Sponge schem v3 is supported. This schematic has version $version")).now
    if version > 3 then
      println(s"Warning: Schematic version $version is newer than supported v3. Attempting to load anyway.")

    val width = schematic("Width").asShort
    val height = schematic("Height").asShort
    val length = schematic("Length").asShort

    val blocksTag = schematic("Blocks").asCompound
    val paletteTag = blocksTag("Palette").asCompound

    val palette = paletteTag.keySet.map:
      case key @ s"$id[$tagStr]" =>
        val attributes = tagStr
          .split(",")
          .map(line =>
            val split = line.split("=")
            (split(0), split(1))
          )
          .toMap

        (paletteTag(key).asInt, Block(id, attributes))

      case id => (paletteTag(id).asInt, Block(id))
    .toMap

    val data = blocksTag("Data").asByteArray
    val blocks = data.to(Chunk).map(id => palette(id.toInt))

    val blockEntitiesTag = blocksTag("BlockEntities").asList.map(_.asCompound)

    val structure = Structure(BlockPos(width, height, length), blocks)

    if blockEntitiesTag == null then structure
    else
      blockEntitiesTag
        .to(Chunk)
        .foldLeft(structure)((struct, entity) =>
          val posArr = entity("Pos").asIntArray
          val pos = BlockPos(posArr(0), posArr(1), posArr(2))

          val dataTag =
            if entity.contains("Data") then entity("Data").asInstanceOf[NBT.CompoundTag]
            else NBT.compound()

          val id = entity("Id").asString

          struct.withBlockEntity(pos, BlockEntity(id, dataTag))
        )

  def saveSponge(structure: Structure): NBT.CompoundTag =
    val blockEntitiesTag = NBT.ListTag(
      structure
        .blocks
        .zipWithIndex
        .filter(_._1.entity.isDefined)
        .map((block, index) =>
          val entity = block.entity.get
          val position = structure.indexToPosition(index)

          NBT.compound(
            "Id" -> NBT.StringTag(entity.id),
            "Pos" -> NBT.intArray(position.x, position.y, position.z),
            "Data" -> entity.data
          )
        )
    )

    val (palette, data) = structure.toPaletteAndData
    val paletteTag = NBT.CompoundTag(palette.map((name, value) => (name, NBT.IntTag(value))))

    NBT.compound(
      "Schematic" -> NBT.compound(
        "DataVersion" -> NBT.IntTag(4671),
        "Version" -> NBT.IntTag(3),
        "Width" -> NBT.ShortTag(structure.dimensions.x.toShort),
        "Height" -> NBT.ShortTag(structure.dimensions.y.toShort),
        "Length" -> NBT.ShortTag(structure.dimensions.z.toShort),
        "Blocks" -> NBT.compound(
          "BlockEntities" -> blockEntitiesTag,
          "Data" -> NBT.ByteArrayTag(data),
          "Palette" -> paletteTag
        )
      )
    )
