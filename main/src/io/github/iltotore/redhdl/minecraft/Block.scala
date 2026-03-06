package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.minecraft.nbt.NBT
import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
import scala.jdk.CollectionConverters.SetHasAsScala

case class Block(id: String, attributes: Map[String, String] = Map.empty, entity: Maybe[BlockEntity] = Absent):

  def toSchemString: String =
    val attributesStr =
      if attributes.isEmpty then ""
      else
        attributes
          .map((k, v) => s"$k=$v")
          .mkString("[", ",", "]")

    s"$id$attributesStr"

  def withData(data: NBT.CompoundTag): Block =
    this.copy(entity = entity.map(_.copy(data = data)))

object Block:

  def withAttributes(id: String)(attributes: (String, String)*): Block =
    Block(id, attributes.toMap)

  val Air: Block = Block("minecraft:air")

  enum Facing derives CanEqual:
    case North, South

    def toRotation: String = this match
      case North => "8"
      case South => "0"

  def Sign(rotation: Facing, messages: String*): Block =
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
