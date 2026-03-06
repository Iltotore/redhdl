package io.github.iltotore.redhdl.minecraft.nbt

import java.io.DataInput
import java.io.DataInputStream
import java.io.DataOutput
import java.io.DataOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.util.zip.GZIPInputStream
import java.util.zip.GZIPOutputStream
import kyo.Chunk
import scala.collection.mutable

enum NBT:
  case ByteTag(value: Byte)
  case ShortTag(value: Short)
  case IntTag(value: Int)
  case LongTag(value: Long)
  case FloatTag(value: Float)
  case DoubleTag(value: Double)
  case ByteArrayTag(values: Chunk[Byte])
  case StringTag(value: String)
  case ListTag(values: Chunk[NBT])
  case CompoundTag(fields: Map[String, NBT])
  case IntArrayTag(values: Chunk[Int])
  case LongArrayTag(values: Chunk[Long])

  def id: Int = ordinal + 1

  def writeGZipped(name: String, stream: OutputStream): Unit =
    val gzip = GZIPOutputStream(stream)
    write(name, DataOutputStream(gzip))
    gzip.finish()

  def write(name: String, stream: DataOutput): Unit =
    stream.writeByte(id)
    stream.writeUTF(name)
    this.writeRaw(stream)

  def writeRaw(stream: DataOutput): Unit = this match
    case ByteTag(value)   => stream.writeByte(value)
    case ShortTag(value)  => stream.writeShort(value)
    case IntTag(value)    => stream.writeInt(value)
    case LongTag(value)   => stream.writeLong(value)
    case FloatTag(value)  => stream.writeFloat(value)
    case DoubleTag(value) => stream.writeDouble(value)
    case ByteArrayTag(values) =>
      stream.writeInt(values.size)
      values.foreach(stream.writeByte(_))
    case StringTag(value) => stream.writeUTF(value)
    case ListTag(values) =>
      stream.writeByte(values.headOption.fold(0)(_.id))
      stream.writeInt(values.size)
      values.foreach(_.writeRaw(stream))
    case CompoundTag(fields) =>
      fields.foreach((name, tag) => tag.write(name, stream))
      stream.writeByte(0)
    case IntArrayTag(values) =>
      stream.writeInt(values.size)
      values.foreach(stream.writeInt)
    case LongArrayTag(values) =>
      stream.writeInt(values.size)
      values.foreach(stream.writeLong)

  def asByte: Byte = this match
    case ByteTag(value) => value
    case _              => throw ClassCastException("NBT is not Byte")

  def asBoolean: Boolean = this match
    case ByteTag(value) => value != 0
    case _              => throw ClassCastException("NBT is not Boolean")

  def asShort: Short = this match
    case ShortTag(value) => value
    case _               => throw ClassCastException("NBT is not Short")

  def asInt: Int = this match
    case IntTag(value) => value
    case _             => throw ClassCastException("NBT is not Int")

  def asLong: Long = this match
    case LongTag(value) => value
    case _              => throw ClassCastException("NBT is not Long")

  def asFloat: Float = this match
    case FloatTag(value) => value
    case _               => throw ClassCastException("NBT is not Float")

  def asDouble: Double = this match
    case DoubleTag(value) => value
    case _                => throw ClassCastException("NBT is not Double")

  def asByteArray: Chunk[Byte] = this match
    case ByteArrayTag(values) => values
    case _                    => throw ClassCastException("NBT is not ByteArray")

  def asString: String = this match
    case StringTag(value) => value
    case _                => throw ClassCastException("NBT is not String")

  def asList: Chunk[NBT] = this match
    case ListTag(values) => values
    case _               => throw ClassCastException("NBT is not a List")

  def asCompound: Map[String, NBT] = this match
    case CompoundTag(fields) => fields
    case _                   => throw ClassCastException("NBT is not a Compound")

  def asIntArray: Chunk[Int] = this match
    case IntArrayTag(values) => values
    case _                   => throw ClassCastException("NBT is not an IntArray")

  def asLongArray: Chunk[Long] = this match
    case LongArrayTag(values) => values
    case _                    => throw ClassCastException("NBT is not a LongArray")

object NBT:

  def boolean(value: Boolean): ByteTag = ByteTag(if value then 0 else 1)

  def list(tags: NBT*): ListTag = ListTag(Chunk.from(tags))

  def compound(entries: (String, NBT)*): CompoundTag = CompoundTag(entries.toMap)

  def readGZipped(stream: InputStream): (String, NBT) =
    read(DataInputStream(GZIPInputStream(stream)))

  def read(stream: DataInput): (String, NBT) =
    val id = stream.readUnsignedByte()
    val name = stream.readUTF()
    (name, readRaw(id, stream))

  def readRaw(id: Int, stream: DataInput): NBT = id match
    case 1 => ByteTag(stream.readByte())
    case 2 => ShortTag(stream.readShort())
    case 3 => IntTag(stream.readInt())
    case 4 => LongTag(stream.readLong())
    case 5 => FloatTag(stream.readFloat())
    case 6 => DoubleTag(stream.readDouble())
    case 7 =>
      val size = stream.readInt()
      val array = new Array[Byte](size)
      stream.readFully(array)
      ByteArrayTag(Chunk.from(array))
    case 8 => StringTag(stream.readUTF())
    case 9 =>
      val elementId = stream.readUnsignedByte()
      val size = stream.readInt()
      ListTag(Chunk.range(0, size).map(_ => readRaw(elementId, stream)))
    case 10 =>
      val fields = mutable.Map.empty[String, NBT]
      var fieldId = 0
      while
        fieldId = stream.readUnsignedByte()
        fieldId != 0
      do
        val name = stream.readUTF()
        fields.update(name, readRaw(fieldId, stream))
      CompoundTag(fields.toMap)
    case 11 =>
      val size = stream.readInt()
      IntArrayTag(Chunk.range(0, size).map(_ => stream.readInt()))
    case 12 =>
      val size = stream.readInt()
      LongArrayTag(Chunk.range(0, size).map(_ => stream.readLong()))

    case _ => throw AssertionError(s"Unexpected NBT id: $id")

  def intArray(values: Int*): IntArrayTag = IntArrayTag(Chunk.from(values))
