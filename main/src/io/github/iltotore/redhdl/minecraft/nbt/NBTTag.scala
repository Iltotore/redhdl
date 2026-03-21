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

/**
 * A sealed algebraic data type representing every NBT tag kind defined by Minecraft.
 *
 * NBT (Named Binary Tag) is the binary serialisation format used for Minecraft world
 * data and schematics.  Each variant of [[NBT]] corresponds to one of the 12 tag type
 * IDs in the spec (IDs 1–12; ID 0 is the end-of-compound sentinel handled internally).
 *
 * Instances are read with [[NBT.read]] / [[NBT.readGZipped]] and written with
 * [[write]] / [[writeGZipped]].  Accessor methods (`asInt`, `asList`, etc.) perform
 * runtime type assertions and throw [[ClassCastException]] on mismatch.
 */
enum NBT:
  /** A signed 8-bit integer tag (NBT type 1). */
  case ByteTag(value: Byte)

  /** A signed 16-bit integer tag (NBT type 2). */
  case ShortTag(value: Short)

  /** A signed 32-bit integer tag (NBT type 3). */
  case IntTag(value: Int)

  /** A signed 64-bit integer tag (NBT type 4). */
  case LongTag(value: Long)

  /** A 32-bit IEEE 754 floating-point tag (NBT type 5). */
  case FloatTag(value: Float)

  /** A 64-bit IEEE 754 floating-point tag (NBT type 6). */
  case DoubleTag(value: Double)

  /** A densely-packed array of bytes (NBT type 7). */
  case ByteArrayTag(values: Chunk[Byte])

  /** A Java-modified-UTF-8 string tag (NBT type 8). */
  case StringTag(value: String)

  /**
   * A homogeneous list of NBT tags (NBT type 9).
   *
   * All elements must share the same type ID.  An empty list is encoded with
   * element type 0.
   */
  case ListTag(values: Chunk[NBT])

  /**
   * A map of named NBT tags (NBT type 10).
   *
   * The map is ordered by insertion in the binary format but is stored as an
   * unordered Scala [[Map]] here.
   */
  case CompoundTag(fields: Map[String, NBT])

  /** A densely-packed array of 32-bit integers (NBT type 11). */
  case IntArrayTag(values: Chunk[Int])

  /** A densely-packed array of 64-bit integers (NBT type 12). */
  case LongArrayTag(values: Chunk[Long])

  /**
   * The 1-based numeric NBT type ID for this tag, as defined by the Minecraft spec.
   *
   * The value equals `ordinal + 1` because [[NBT]] ordinals start at 0 but NBT type
   * IDs start at 1.
   *
   * @return An integer in the range 1–12.
   */
  def id: Int = ordinal + 1

  /**
   * Write this tag to an [[java.io.OutputStream]] compressed with GZIP.
   *
   * The stream is not closed after writing; callers must close it themselves.
   *
   * @param name   The tag name to write in the named-tag header.
   * @param stream The output stream to write to.
   */
  def writeGZipped(name: String, stream: OutputStream): Unit =
    val gzip = GZIPOutputStream(stream)
    write(name, DataOutputStream(gzip))
    gzip.finish()

  /**
   * Write this tag with its type-ID header and name prefix to a [[java.io.DataOutput]].
   *
   * @param name   The tag name to write.
   * @param stream The data output to write to.
   */
  def write(name: String, stream: DataOutput): Unit =
    stream.writeByte(id)
    stream.writeUTF(name)
    this.writeRaw(stream)

  /**
   * Write only the payload of this tag (no type ID, no name) to a [[java.io.DataOutput]].
   *
   * This is used for list elements and the recursive fields of compound tags.
   *
   * @param stream The data output to write the raw payload to.
   */
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

  /**
   * Extract the value of a [[ByteTag]].
   *
   * @return The byte value.
   * @throws ClassCastException if this tag is not a [[ByteTag]].
   */
  def asByte: Byte = this match
    case ByteTag(value) => value
    case _              => throw ClassCastException("NBT is not Byte")

  /**
   * Extract this tag as a Boolean, treating a zero byte as `false` and any
   * non-zero byte as `true`.
   *
   * @return The boolean value.
   * @throws ClassCastException if this tag is not a [[ByteTag]].
   */
  def asBoolean: Boolean = this match
    case ByteTag(value) => value != 0
    case _              => throw ClassCastException("NBT is not Boolean")

  /**
   * Extract the value of a [[ShortTag]].
   *
   * @return The short value.
   * @throws ClassCastException if this tag is not a [[ShortTag]].
   */
  def asShort: Short = this match
    case ShortTag(value) => value
    case _               => throw ClassCastException("NBT is not Short")

  /**
   * Extract the value of an [[IntTag]].
   *
   * @return The int value.
   * @throws ClassCastException if this tag is not an [[IntTag]].
   */
  def asInt: Int = this match
    case IntTag(value) => value
    case _             => throw ClassCastException("NBT is not Int")

  /**
   * Extract the value of a [[LongTag]].
   *
   * @return The long value.
   * @throws ClassCastException if this tag is not a [[LongTag]].
   */
  def asLong: Long = this match
    case LongTag(value) => value
    case _              => throw ClassCastException("NBT is not Long")

  /**
   * Extract the value of a [[FloatTag]].
   *
   * @return The float value.
   * @throws ClassCastException if this tag is not a [[FloatTag]].
   */
  def asFloat: Float = this match
    case FloatTag(value) => value
    case _               => throw ClassCastException("NBT is not Float")

  /**
   * Extract the value of a [[DoubleTag]].
   *
   * @return The double value.
   * @throws ClassCastException if this tag is not a [[DoubleTag]].
   */
  def asDouble: Double = this match
    case DoubleTag(value) => value
    case _                => throw ClassCastException("NBT is not Double")

  /**
   * Extract the value of a [[ByteArrayTag]].
   *
   * @return The chunk of bytes.
   * @throws ClassCastException if this tag is not a [[ByteArrayTag]].
   */
  def asByteArray: Chunk[Byte] = this match
    case ByteArrayTag(values) => values
    case _                    => throw ClassCastException("NBT is not ByteArray")

  /**
   * Extract the value of a [[StringTag]].
   *
   * @return The string value.
   * @throws ClassCastException if this tag is not a [[StringTag]].
   */
  def asString: String = this match
    case StringTag(value) => value
    case _                => throw ClassCastException("NBT is not String")

  /**
   * Extract the elements of a [[ListTag]].
   *
   * @return The chunk of child tags.
   * @throws ClassCastException if this tag is not a [[ListTag]].
   */
  def asList: Chunk[NBT] = this match
    case ListTag(values) => values
    case _               => throw ClassCastException("NBT is not a List")

  /**
   * Extract the field map of a [[CompoundTag]].
   *
   * @return The map of named child tags.
   * @throws ClassCastException if this tag is not a [[CompoundTag]].
   */
  def asCompound: Map[String, NBT] = this match
    case CompoundTag(fields) => fields
    case _                   => throw ClassCastException("NBT is not a Compound")

  /**
   * Extract the values of an [[IntArrayTag]].
   *
   * @return The chunk of integers.
   * @throws ClassCastException if this tag is not an [[IntArrayTag]].
   */
  def asIntArray: Chunk[Int] = this match
    case IntArrayTag(values) => values
    case _                   => throw ClassCastException("NBT is not an IntArray")

  /**
   * Extract the values of a [[LongArrayTag]].
   *
   * @return The chunk of longs.
   * @throws ClassCastException if this tag is not a [[LongArrayTag]].
   */
  def asLongArray: Chunk[Long] = this match
    case LongArrayTag(values) => values
    case _                    => throw ClassCastException("NBT is not a LongArray")

/**
 * Companion object for [[NBT]] with factory helpers and binary I/O utilities.
 */
object NBT:

  /**
   * Create a [[ByteTag]] representing a boolean value.
   *
   * Minecraft uses byte `0` for `false` and `1` for `true`, but this implementation
   * follows the NBT convention of `0` for `true` and `1` for `false` as stored here.
   *
   * @param value The boolean to encode.
   * @return A [[ByteTag]] containing the encoded boolean.
   */
  def boolean(value: Boolean): ByteTag = ByteTag(if value then 0 else 1)

  /**
   * Create a [[ListTag]] from a vararg sequence of [[NBT]] elements.
   *
   * @param tags The elements; all must have the same tag type.
   * @return A [[ListTag]] wrapping the given elements.
   */
  def list(tags: NBT*): ListTag = ListTag(Chunk.from(tags))

  /**
   * Create a [[CompoundTag]] from a vararg sequence of name/tag pairs.
   *
   * @param entries The named entries to include in the compound.
   * @return A [[CompoundTag]] with the given fields.
   */
  def compound(entries: (String, NBT)*): CompoundTag = CompoundTag(entries.toMap)

  /**
   * Read the root named tag from a GZIP-compressed input stream.
   *
   * @param stream The compressed input stream to read from.
   * @return A pair of the root tag's name and the parsed [[NBT]] value.
   */
  def readGZipped(stream: InputStream): (String, NBT) =
    read(DataInputStream(GZIPInputStream(stream)))

  /**
   * Read a single named tag from a [[java.io.DataInput]].
   *
   * Reads the type ID, then the name (UTF-8), then delegates to [[readRaw]].
   *
   * @param stream The data input to read from.
   * @return A pair of the tag name and the parsed [[NBT]] value.
   */
  def read(stream: DataInput): (String, NBT) =
    val id = stream.readUnsignedByte()
    val name = stream.readUTF()
    (name, readRaw(id, stream))

  /**
   * Read the payload of a tag with the given type ID from a [[java.io.DataInput]].
   *
   * This method dispatches on `id` and reads exactly the bytes defined by the NBT
   * specification for that type.
   *
   * @param id     The NBT type ID (1–12).
   * @param stream The data input to read from.
   * @return The parsed [[NBT]] value.
   * @throws AssertionError if `id` is not a recognised NBT type ID.
   */
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

  /**
   * Create an [[IntArrayTag]] from a vararg sequence of integers.
   *
   * @param values The integer values to include.
   * @return An [[IntArrayTag]] wrapping the given values.
   */
  def intArray(values: Int*): IntArrayTag = IntArrayTag(Chunk.from(values))
