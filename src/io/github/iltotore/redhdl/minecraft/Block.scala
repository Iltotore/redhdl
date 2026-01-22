package io.github.iltotore.redhdl.minecraft

import io.github.ensgijs.nbt.tag.CompoundTag
import io.github.ensgijs.nbt.tag.ListTag
import io.github.ensgijs.nbt.tag.Tag
import kyo.Absent
import kyo.Maybe
import kyo.Present
import scala.jdk.CollectionConverters.SetHasAsScala
import io.github.ensgijs.nbt.tag.StringTag
import kyo.Chunk

case class Block(id: String, attributes: Map[String, String] = Map.empty, entity: Maybe[BlockEntity] = Absent):

  def toSchemString: String =
    val attributesStr =
      if attributes.isEmpty then ""
      else
        attributes
          .map((k, v) => s"$k=$v")
          .mkString("[", ",", "]")

    s"$id$attributesStr"

  def withData(data: CompoundTag): Block =
    this.copy(entity = entity.map(_.copy(data = data)))

object Block:

  def withAttributes(id: String)(attributes: (String, String)*): Block =
    Block(id, attributes.toMap)

  val Air: Block = Block("minecraft:air")

  def Sign(messages: String*): Block =
    val messageLines = messages ++ Chunk.fill(math.max(0, 4 - messages.length))("")
    val frontMessagesTag = ListTag(classOf[StringTag])
    for msg <- messageLines do frontMessagesTag.addString(msg)

    val frontText = CompoundTag()
    frontText.put("messages", frontMessagesTag)

    val data = CompoundTag()
    data.put("front_text", frontText)

    Block(
      id = "minecraft:oak_sign",
      attributes = Map("facing" -> "north"),
      entity = Present(BlockEntity("minecraft:sign", data))
    )
