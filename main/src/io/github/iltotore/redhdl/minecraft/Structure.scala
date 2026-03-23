package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.minecraft.nbt.NBT
import java.io.IOException
import kyo.*
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.jdk.CollectionConverters.SetHasAsScala

/**
 * An immutable 3-D grid of [[Block]]s representing a Minecraft structure.
 *
 * Blocks are stored in a flat chunk indexed as `x + z * width + y * width * length`
 * (XZY order), matching the Sponge schematic format.  Coordinates outside the bounding
 * box are silently rejected with a warning printed to stdout.
 *
 * The primary way to create a structure is [[Structure.empty]]; blocks are placed with
 * the various `with*` methods, all of which return a new copy.
 *
 * @param dimensions The bounding box of this structure as a [[BlockPos]] `(width, height, length)`.
 * @param blocks     The flat block array in XZY order.  Its size must equal
 *                   `dimensions.x * dimensions.y * dimensions.z`.
 */
case class Structure(dimensions: BlockPos, var blocks: Chunk[Block]):

  /** The width of this structure along the X axis. */
  def width: Int = dimensions.x

  /** The height of this structure along the Y axis. */
  def height: Int = dimensions.y

  /** The length of this structure along the Z axis. */
  def length: Int = dimensions.z

  /**
   * Convert a [[BlockPos]] to a flat array index (XZY encoding).
   *
   * @param position The block position to convert.
   * @return The corresponding index into [[blocks]].
   */
  private def positionToIndex(position: BlockPos): Int =
    position.x + position.z * width + position.y * width * length

  /**
   * Convert a flat array index back to a [[BlockPos]] (XZY decoding).
   *
   * @param index The flat array index.
   * @return The corresponding [[BlockPos]].
   */
  private def indexToPosition(index: Int): BlockPos =
    val y = index / (width * length)
    val z = (index - y * width * length) / width
    val x = index - z * width - y * width * length

    BlockPos(x, y, z)

  /**
   * Read the block at the given position.
   *
   * @param position The position to query.
   * @return The [[Block]] at `position`.
   */
  def apply(position: BlockPos): Block = blocks(positionToIndex(position))

  /**
   * Test whether a [[BlockPos]] lies within the bounds of this structure.
   *
   * @param pos The position to test.
   * @return `true` if all coordinates are non-negative and less than the corresponding
   *         dimension.
   */
  private def inBounds(pos: BlockPos): Boolean =
    pos.x >= 0 && pos.y >= 0 && pos.z >= 0 &&
      pos.x < width && pos.y < height && pos.z < length

  /**
   * Return a copy of this structure with the block at `position` replaced.
   *
   * If `position` is out-of-bounds a warning is printed and `this` is returned
   * unchanged.  If the target position already contains a non-air block and
   * `overrideBlock` is `false`, the existing block is kept.
   *
   * @param position      The target position.
   * @param block         The block to place.
   * @param overrideBlock When `true`, replace existing non-air blocks; otherwise
   *                      air blocks only (default `false`).
   * @return The updated structure.
   */
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

  /**
   * Draw a horizontal line of blocks along the X axis.
   *
   * @param positionA     The starting position (inclusive).
   * @param to            The ending X coordinate (inclusive).
   * @param block         The block to place at each position.
   * @param overrideBlock When `true`, replace existing non-air blocks.
   * @return The updated structure.
   */
  def withLineX(positionA: BlockPos, to: Int, block: Block, overrideBlock: Boolean = false): Structure =
    val step = if to >= positionA.x then 1 else -1
    Range.inclusive(positionA.x, to, step).foldLeft(this)((structure, x) =>
      structure.withBlock(BlockPos(x, positionA.y, positionA.z), block, overrideBlock)
    )

  /**
   * Draw a line of blocks along the Z axis.
   *
   * @param positionA     The starting position (inclusive).
   * @param to            The ending Z coordinate (inclusive).
   * @param block         The block to place at each position.
   * @param overrideBlock When `true`, replace existing non-air blocks.
   * @return The updated structure.
   */
  def withLineZ(positionA: BlockPos, to: Int, block: Block, overrideBlock: Boolean = false): Structure =
    val step = if to >= positionA.z then 1 else -1
    Range.inclusive(positionA.z, to, step).foldLeft(this)((structure, z) =>
      structure.withBlock(BlockPos(positionA.x, positionA.y, z), block, overrideBlock)
    )

  /**
   * Replace the NBT data of the block entity at `position`.
   *
   * @param position The target block position.
   * @param data     The new [[NBT.CompoundTag]] to store.
   * @return The updated structure.
   */
  def withData(position: BlockPos, data: NBT.CompoundTag): Structure =
    this.withBlock(position, this(position).withData(data))

  /**
   * Attach a [[BlockEntity]] to the block at `position`.
   *
   * @param position The target block position.
   * @param entity   The [[BlockEntity]] to attach.
   * @return The updated structure.
   */
  def withBlockEntity(position: BlockPos, entity: BlockEntity): Structure =
    this.withBlock(position, this(position).copy(entity = Present(entity)))

  /**
   * Paste another structure into this one at the given offset.
   *
   * Each block of the source structure is placed using [[withBlock]] (air blocks in
   * the source do not overwrite existing blocks at the destination).
   *
   * @param position  The origin offset at which to paste the source structure.
   * @param structure The structure to paste.
   * @return The updated structure.
   */
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
   * with the supplied colour.
   *
   * This is used when pasting gate schematics so that the graphic can be tinted
   * according to the surrounding wire palette.
   *
   * @param color The replacement block for `minecraft:white_wool`.
   * @return A new structure with recoloured wool blocks.
   */
  def recolor(color: Block): Structure =
    val newBlocks = blocks.map(b =>
      if b.id == "minecraft:white_wool" then color
      else b
    )
    this.copy(blocks = newBlocks)

  /**
   * Build the block-palette map and the corresponding data byte array required
   * by the Sponge schematic format.
   *
   * Each unique [[Block.toSchemString]] value is assigned a sequential integer ID;
   * the data array stores one ID per block position in XZY order.
   *
   * @return A pair of:
   *         - A map from schematic string to palette index.
   *         - A [[kyo.Chunk]] of palette indices (one per block), as bytes.
   */
  def toPaletteAndData: (Map[String, Int], Chunk[Byte]) =
    val (_, finalPalette, finalData) = blocks.foldLeft((0.toByte, Map.empty[String, Int], Chunk.empty[Byte])):
      case ((nextId, palette, data), block) =>
        val str = block.toSchemString
        if palette.contains(str) then (nextId, palette, data :+ palette(str).toByte)
        else ((nextId + 1).toByte, palette.updated(str, nextId), data :+ nextId)

    (finalPalette, finalData)

/**
 * Factory methods and Sponge schematic serialisers/deserialisers for [[Structure]].
 */
object Structure:

  /**
   * Create an empty structure of the given dimensions, filled with `fill` blocks.
   *
   * @param dimensions The bounding box `(width, height, length)`.
   * @param fill       The block to fill with (default: [[Block.Air]]).
   * @return A new [[Structure]] filled uniformly with `fill`.
   */
  def empty(dimensions: BlockPos, fill: Block = Block.Air): Structure =
    Structure(
      dimensions = dimensions,
      blocks = Chunk.fill(dimensions.x * dimensions.y * dimensions.z)(fill)
    )

  /**
   * Load a [[Structure]] from the root NBT tag of a Sponge schematic (v3).
   *
   * The method validates the schematic version, parses palette, block data, and block
   * entities, and assembles them into a [[Structure]].
   *
   * @param path The resource or file path string used in error messages.
   * @param root The top-level [[NBT]] tag (the `Schematic` root compound).
   * @return The parsed [[Structure]] within `Abort[SchematicFailure]`, failing with
   *         [[SchematicFailure.InvalidSchematic]] if the version is not supported.
   */
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

  /**
   * Serialise a [[Structure]] to a Sponge schematic v3 NBT compound tag.
   *
   * The resulting tag can be written to disk via [[NBT.writeGZipped]].
   *
   * @param structure The structure to serialise.
   * @return A [[NBT.CompoundTag]] containing the full `Schematic` hierarchy.
   */
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
