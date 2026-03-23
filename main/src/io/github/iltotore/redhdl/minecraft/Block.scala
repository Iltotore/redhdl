package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.minecraft.nbt.NBT
import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import scala.jdk.CollectionConverters.SetHasAsScala

/**
 * A single Minecraft block, consisting of a namespaced block ID, optional block-state
 * attributes, and an optional block entity.
 *
 * [[Block]] values are the atomic unit of a [[Structure]].  They are serialised
 * to the Sponge schematic format via [[toSchemString]] (for the palette) and their
 * entity data is embedded directly in the NBT.
 *
 * @param id         The namespaced block identifier (e.g. `"minecraft:stone"`).
 * @param attributes A map of block-state key/value pairs (e.g. `"facing" -> "north"`).
 *                   An empty map means no block-state is encoded.
 * @param entity     An optional [[BlockEntity]] attached to this block.
 */
case class Block(id: String, attributes: Map[String, String] = Map.empty, entity: Maybe[BlockEntity] = Absent):

  /**
   * Serialise this block to the string representation used in Sponge schematic palettes.
   *
   * The format is `<id>[<key>=<value>,...]`; the attribute list is omitted entirely
   * when there are no attributes.
   *
   * @return A schematic-compatible palette string for this block.
   */
  def toSchemString: String =
    val attributesStr =
      if attributes.isEmpty then ""
      else
        attributes
          .map((k, v) => s"$k=$v")
          .mkString("[", ",", "]")

    s"$id$attributesStr"

  /**
   * Return a copy of this block with its block-entity data replaced.
   *
   * If the block has no entity this method is a no-op.
   *
   * @param data The new [[NBT.CompoundTag]] to store in the block entity.
   * @return A new [[Block]] whose entity carries `data`.
   */
  def withData(data: NBT.CompoundTag): Block =
    this.copy(entity = entity.map(_.copy(data = data)))

/**
 * Factory helpers and pre-built [[Block]] constants used throughout the schematic
 * generator.
 */
object Block:

  /**
   * Create a [[Block]] with the given ID and an arbitrary number of block-state
   * attributes supplied as varargs pairs.
   *
   * @param id         The namespaced block identifier.
   * @param attributes Block-state key/value pairs.
   * @return A new [[Block]] with the given ID and attributes.
   */
  def withAttributes(id: String)(attributes: (String, String)*): Block =
    Block(id, attributes.toMap)

  /** The Minecraft air block (`minecraft:air`), used as the default fill value. */
  val Air: Block = Block("minecraft:air")

  /**
   * The two cardinal orientations used for sign blocks placed on top of other blocks.
   *
   * The [[toRotation]] method converts each variant to the `rotation` block-state value
   * expected by the Minecraft sign format.
   */
  enum Rotation derives CanEqual:
    /** Sign facing north (rotation value `"8"`). */
    case North

    /** Sign facing south (rotation value `"0"`). */
    case South

    /**
     * Convert this [[Rotation]] to the corresponding Minecraft block-state `rotation` value.
     *
     * @return The string rotation value for the block state.
     */
    def toRotation: String = this match
      case North => "8"
      case South => "0"

  /**
   * The four cardinal directions used as `facing` block-state values for repeaters
   * and other directional blocks.
   */
  enum Facing derives CanEqual:
    case North, South, East, West

    /**
     * Convert this [[Facing]] to the lowercase string expected by Minecraft block states.
     *
     * @return The lowercase facing string.
     */
    def toFacing: String = this.toString.toLowerCase

  /**
   * Create a standing sign block with up to four lines of text.
   *
   * Missing lines are padded with empty strings so the NBT always contains exactly
   * four message entries.
   *
   * @param rotation The direction the sign faces.
   * @param messages Up to four lines of text to display on the sign.
   * @return A [[Block]] representing an oak sign with the given text.
   */
  def Sign(rotation: Rotation, messages: String*): Block =
    val messageLines = messages ++ Chunk.fill(math.max(0, 4 - messages.length))("")

    Block(
      id = "minecraft:oak_sign",
      attributes = Map("rotation" -> rotation.toRotation),
      entity = Present(BlockEntity(
        id = "minecraft:sign",
        data = NBT.compound(
          "front_text" -> NBT.compound(
            "messages" -> NBT.list(messageLines.map(NBT.StringTag.apply)*)
          )
        )
      ))
    )

  /**
   * Create a redstone repeater block with the given delay and facing direction.
   *
   * @param delay  The repeater delay in ticks (1–4).
   * @param facing The direction the repeater is facing (default: North).
   * @return A [[Block]] representing a placed repeater.
   */
  def Repeater(delay: RepeaterDelay, facing: Facing = Facing.North): Block =
    withAttributes("minecraft:repeater")(
      "delay" -> delay.toString,
      "facing" -> facing.toFacing
    )

  /** A plain redstone wire block (`minecraft:redstone_wire`) with no attributes. */
  val RedstoneWire: Block = Block("minecraft:redstone_wire")
