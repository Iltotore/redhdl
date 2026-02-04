package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.graph.NodeType
import kyo.*
import scala.util.Using
import java.nio.file.Files
import java.util.function.Supplier
import java.io.InputStream
import java.io.IOException
import io.github.ensgijs.nbt.io.BinaryNbtHelpers
import io.github.ensgijs.nbt.io.CompressionType

case class SchematicContext(schematics: Map[GateType, Structure]):

  def getSchematic(tpe: GateType): Maybe[Structure] = Maybe.fromOption(schematics.get(tpe))

object SchematicContext:

  def load(types: Chunk[GateType]): SchematicContext < (Abort[SchematicFailure] & Sync) =
    Kyo.foreach(types)(tpe =>
        val resourcePath = s"/gates/${tpe.resourceName}.schem"
        val resourceInput = getClass.getResourceAsStream(resourcePath)

        if resourceInput == null then Abort.fail(SchematicFailure.MissingSchematic(tpe))
        else
          Using.resource(resourceInput)(input =>
            direct:
              (
                tpe,
                Structure.loadSponge(
                  path = s"internal:$resourcePath",
                  root = BinaryNbtHelpers.read(input, CompressionType.GZIP).getTagAutoCast
                ).now
              )
          )
    ).map(pairs => SchematicContext(pairs.toMap))

  def getSchematic(tpe: GateType): Structure < SchematicGeneration =
    Env.use(ctx =>
      ctx.getSchematic(tpe) match
        case Present(value) => value
        case Absent => Abort.fail(SchematicFailure.MissingSchematic(tpe))
    )

  def getDimensions(tpe: GateType): BlockPos < SchematicGeneration =
    getSchematic(tpe).map(_.dimensions)