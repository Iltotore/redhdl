package io.github.iltotore.redhdl.minecraft

import kyo.*
import java.io.IOException
import io.github.ensgijs.nbt.tag.CompoundTag
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.jdk.CollectionConverters.ListHasAsScala
import io.github.ensgijs.nbt.tag.DoubleTag
import io.github.ensgijs.nbt.tag.IntTag
import io.github.ensgijs.nbt.tag.StringTag
import io.github.ensgijs.nbt.tag.ByteTag
import io.github.ensgijs.nbt.tag.ListTag

case class Structure(dimensions: BlockPos, blocks: Chunk[Block]):

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

  def withBlock(position: BlockPos, block: Block): Structure =
    this.copy(blocks = blocks.updated(positionToIndex(position), block))

  def withLineX(positionA: BlockPos, to: Int, block: Block): Structure =
    val step = if to >= positionA.x then 1 else -1
    Range.inclusive(positionA.x, to, step).foldLeft(this)((structure, x) =>
      structure.withBlock(BlockPos(x, positionA.y, positionA.z), block)
    )
  
  def withLineZ(positionA: BlockPos, to: Int, block: Block): Structure =
    val step = if to >= positionA.z then 1 else -1
    Range.inclusive(positionA.z, to, step).foldLeft(this)((structure, z) =>
      structure.withBlock(BlockPos(positionA.x, positionA.y, z), block)
    )

  def withData(position: BlockPos, data: CompoundTag): Structure =
    this.withBlock(position, this(position).withData(data))

  def withBlockEntity(position: BlockPos, entity: BlockEntity): Structure =
    this.withBlock(position, this(position).copy(entity = Present(entity)))

  def withStructure(position: BlockPos, structure: Structure): Structure =
    val positions =
      for
        x <- 0 until structure.width
        y <- 0 until structure.height
        z <- 0 until structure.length
      yield
        BlockPos(x, y, z)

    positions.foldLeft(this)((result, pos) => result.withBlock(position + pos, structure(pos)))

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

  def loadSponge(path: String, root: CompoundTag): Structure < Abort[SchematicFailure] = direct:
    val schematic = root.getCompoundTag("Schematic")
    val version = schematic.getInt("Version")
    if version < 3 then
      Abort.fail(SchematicFailure.InvalidSchematic(path, s"Only Sponge schem v3 is supported. This schematic has version $version")).now
    if version > 3 then
      println(s"Warning: Schematic version $version is newer than supported v3. Attempting to load anyway.")
    
    val width = schematic.getShort("Width").toInt
    val height = schematic.getShort("Height").toInt
    val length = schematic.getShort("Length").toInt

    val blocksTag = schematic.getCompoundTag("Blocks")
    val paletteTag = blocksTag.getCompoundTag("Palette")

    val palette = paletteTag.keySet().asScala.map:
      case key@s"$id[$tagStr]" =>
        val tag = CompoundTag()

        val attributes = tagStr
          .split(",")
          .map(line =>
            val split = line.split("=")
            (split(0), split(1))  
          )
          .toMap

        (paletteTag.getInt(key), Block(id, attributes))
    
      case id => (paletteTag.getInt(id), Block(id))
    .toMap

    val data = blocksTag.getByteArray("Data")
    val blocks = data.to(Chunk).map(id => palette(id.toInt))

    val blockEntitiesTag = blocksTag.getCompoundList("BlockEntities")

    val structure = Structure(BlockPos(width, height, length), blocks)

    if blockEntitiesTag == null then structure
    else
      blockEntitiesTag
        .asScala
        .to(Chunk)
        .foldLeft(structure)((struct, entity) =>
          val posArr = entity.getIntArray("Pos")
          val pos = BlockPos(posArr(0), posArr(1), posArr(2))

          val dataTag =
            if entity.containsKey("Data") then entity.getCompoundTag("Data")
            else CompoundTag()

          val id = entity.getString("Id")

          struct.withBlockEntity(pos, BlockEntity(id, dataTag))
        )

  def saveSponge(structure: Structure): CompoundTag =
    val blockEntitiesTag = ListTag(classOf[CompoundTag])

    for (block, index) <- structure.blocks.zipWithIndex if block.entity.isDefined do
      val entity = block.entity.get
      val position = structure.indexToPosition(index)

      val entityTag = CompoundTag()
      entityTag.putString("Id", entity.id)
      entityTag.putIntArray("Pos", Array(position.x, position.y, position.z))
      entityTag.put("Data", entity.data)

      blockEntitiesTag.add(entityTag)

    val (palette, data) = structure.toPaletteAndData

    val paletteTag = CompoundTag()
    for (key, value) <- palette do paletteTag.putInt(key, value)

    val blocksTag = CompoundTag()
    blocksTag.put("BlockEntities", blockEntitiesTag)
    blocksTag.putByteArray("Data", data.toArray)
    blocksTag.put("Palette", paletteTag)


    val schematicTag = CompoundTag()
    schematicTag.putInt("DataVersion", 4671) //1.21.11
    schematicTag.putInt("Version", 3)

    schematicTag.putShort("Width", structure.dimensions.x.toShort)
    schematicTag.putShort("Height", structure.dimensions.y.toShort)
    schematicTag.putShort("Length", structure.dimensions.z.toShort)

    schematicTag.put("Blocks", blocksTag)

    val root = CompoundTag()
    root.put("Schematic", schematicTag)

    root