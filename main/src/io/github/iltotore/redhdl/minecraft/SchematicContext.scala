package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.graph.NodeType
import io.github.iltotore.redhdl.graph.PinX
import io.github.iltotore.redhdl.minecraft.nbt.NBT
import java.io.IOException
import java.io.InputStream
import java.nio.file.Files
import java.util.function.Supplier
import kyo.*
import scala.util.Using

case class SchematicContext(
    schematics: Map[GateType, Structure],
    palette: Chunk[Block],
    repeaterDelay: RepeaterDelay
):

  def getSchematic(tpe: GateType): Maybe[Structure] = Maybe.fromOption(schematics.get(tpe))

object SchematicContext:

  /**
   * Load all internal schematics and build the context.  A palette of blocks can be
   * provided to colour wires; if nothing is supplied the default palette is used.
   */
  def load(
      types: Chunk[GateType],
      palette: Chunk[Block] = Palette.default,
      repeaterDelay: RepeaterDelay = RepeaterDelay(1)
  ): SchematicContext < (Abort[SchematicFailure] & Sync) =
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
                root = NBT.readGZipped(input)._2
              ).now
            )
        )
    ).map(pairs => SchematicContext(pairs.toMap, palette, repeaterDelay))

  /**
   * Getter pour la palette actuelle dans l'environnement.
   */
  def paletteBlocks: Chunk[Block] < SchematicGeneration =
    Env.use[SchematicContext](_.palette)

  def getSchematic(tpe: GateType): Structure < SchematicGeneration =
    Env.use(ctx =>
      ctx.getSchematic(tpe) match
        case Present(value) => value
        case Absent         => Abort.fail(SchematicFailure.MissingSchematic(tpe))
    )

  def getDimensions(tpe: GateType): BlockPos < SchematicGeneration =
    getSchematic(tpe).map(_.dimensions)

  /**
   * Choose a block from the context palette using only the horizontal pin index.
   * When there are more pins than colours we wrap around the list.
   */
  def getPaletteBlock(pin: PinX): Block < SchematicGeneration =
    Env.use(ctx =>
      ctx.palette match
        case Chunk() => Palette.default.head
        case palette => palette((pin.value % palette.size).toInt)
    )

  def getRepeaterDelay: RepeaterDelay < SchematicGeneration =
    Env.use(_.repeaterDelay)
