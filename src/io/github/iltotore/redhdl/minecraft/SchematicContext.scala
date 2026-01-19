package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.graph.NodeType
import com.sk89q.worldedit.extent.clipboard.Clipboard
import kyo.*
import java.nio.file.Path as JPath
import scala.util.Using
import java.nio.file.Files
import com.sk89q.worldedit.extent.clipboard.io.ClipboardFormat
import com.sk89q.worldedit.extent.clipboard.io.ClipboardFormats
import java.util.function.Supplier
import java.io.InputStream
import java.io.IOException
import com.sk89q.worldedit.math.BlockVector3

case class SchematicContext(schematics: Map[GateType, Clipboard]):

  def getSchematic(tpe: GateType): Maybe[Clipboard] = Maybe.fromOption(schematics.get(tpe))

object SchematicContext:

  def load(types: Chunk[GateType]): SchematicContext < Sync =
    val schematics = types.map(tpe =>
        val inputSupplier: Supplier[InputStream] = () => getClass.getResourceAsStream(s"/gates/${tpe.resourceName}.schem")
        val format = ClipboardFormats.findByInputStream(inputSupplier)
        Using.resource(inputSupplier.get())( input =>
          (tpe, format.getReader(input).read())
        )
    )
    
    SchematicContext(schematics.toMap)

  def getSchematic(tpe: GateType): Clipboard < SchematicGeneration =
    Env.use(ctx =>
      ctx.getSchematic(tpe) match
        case Present(value) => value
        case Absent => Abort.fail(SchematicFailure.MissingSchematic(tpe))
    )

  def getDimensions(tpe: GateType): BlockVector3 < SchematicGeneration =
    getSchematic(tpe).map(_.getDimensions())