package io.github.iltotore.redhdl.minecraft

import io.github.ensgijs.nbt.tag.CompoundTag
import kyo.Chunk
import io.github.ensgijs.nbt.tag.ListTag

case class BlockEntity(id: String, data: CompoundTag)

/*object BlockEntity:

  def sign(position: BlockPos)(messages: String*): BlockEntity =
    val frontMessagesTag = ListTag(classOf[String])
    for msg <- messages do frontMessagesTag.addString(msg)
    
    val data = CompoundTag()
    data.put("front_text", data)*/