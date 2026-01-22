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

  def load(types: Chunk[GateType]): SchematicContext < (Abort[SchematicFailure] & Sync) =
    Kyo.foreach(types)(tpe =>
      direct:
        val resourcePath = s"/gates/${tpe.resourceName}.schem"

        // Using a Supplier to allow multiple reads for format detection and actual reading
        val inputSupplier: Supplier[InputStream] = () => getClass.getResourceAsStream(resourcePath)
        val input = inputSupplier.get()
        if input == null then Abort.fail(SchematicFailure.MissingSchematic(tpe)).now

        // Detect the schematic format
        val format = ClipboardFormats.findByInputStream(inputSupplier)
        
        if format == null then Abort.fail(SchematicFailure.InvalidSchematic(resourcePath, "")).now
        else
          // Read the schematic using the detected format
          Using.resource(input)( input =>
            (tpe, format.getReader(input).read())
          )
    ).map(pairs => SchematicContext(pairs.toMap))

  def getSchematic(tpe: GateType): Clipboard < SchematicGeneration =
    Env.use(ctx =>
      ctx.getSchematic(tpe) match
        case Present(value) => value
        case Absent => Abort.fail(SchematicFailure.MissingSchematic(tpe))
    )

  def getDimensions(tpe: GateType): BlockVector3 < SchematicGeneration =
    getSchematic(tpe).map(_.getDimensions())