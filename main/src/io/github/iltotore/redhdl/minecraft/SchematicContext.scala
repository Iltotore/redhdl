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

/**
 * The runtime environment for schematic generation.
 *
 * [[SchematicContext]] bundles all configuration and pre-loaded data that the
 * [[SchematicGenerator]] needs to produce a [[Structure]]:
 *   - The gate schematics read from bundled resources.
 *   - The wire colour palette.
 *   - The repeater delay setting.
 *
 * An instance is injected into [[SchematicGeneration]] computations via the
 * [[kyo.Env]] effect.
 *
 * @param schematics    A map from [[GateType]] to the corresponding pre-loaded
 *                      [[Structure]] read from the bundled resource files.
 * @param palette       The ordered list of blocks used to colour wire segments.
 * @param repeaterDelay The delay ticks to set on every generated redstone repeater.
 */
case class SchematicContext(
    schematics: Map[GateType, Structure],
    palette: Chunk[Block],
    repeaterDelay: RepeaterDelay
):

  /**
   * Look up the schematic for a specific gate type.
   *
   * @param tpe The gate type whose schematic is requested.
   * @return [[kyo.Present]] with the [[Structure]] if available, [[kyo.Absent]] otherwise.
   */
  def getSchematic(tpe: GateType): Maybe[Structure] = Maybe.fromOption(schematics.get(tpe))

/**
 * Factory methods and [[SchematicGeneration]] accessors for [[SchematicContext]].
 */
object SchematicContext:

  /**
   * Load all internal gate schematics and build the context.
   *
   * Each gate type in `types` is resolved to a bundled resource at
   * `/gates/<resourceName>.schem`.  Resources are read as GZipped Sponge v3
   * schematics.
   *
   * @param types         The gate types whose schematics should be loaded.
   * @param palette       The wire colour palette (defaults to [[Palette.default]]).
   * @param repeaterDelay The repeater delay to use in generated circuits (default 1).
   * @return A [[SchematicContext]] within `Abort[SchematicFailure] & Sync`, failing with
   *         [[SchematicFailure.MissingSchematic]] if a resource cannot be found or
   *         [[SchematicFailure.InvalidSchematic]] if parsing fails.
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
   * Read the wire colour palette from the ambient [[SchematicContext]].
   *
   * @return The palette [[kyo.Chunk]] within a [[SchematicGeneration]] computation.
   */
  def paletteBlocks: Chunk[Block] < SchematicGeneration =
    Env.use[SchematicContext](_.palette)

  /**
   * Retrieve the pre-loaded [[Structure]] schematic for a given gate type from the
   * ambient [[SchematicContext]], failing if no schematic is registered.
   *
   * @param tpe The gate type whose schematic is requested.
   * @return The [[Structure]] within a [[SchematicGeneration]] computation.
   */
  def getSchematic(tpe: GateType): Structure < SchematicGeneration =
    Env.use(ctx =>
      ctx.getSchematic(tpe) match
        case Present(value) => value
        case Absent         => Abort.fail(SchematicFailure.MissingSchematic(tpe))
    )

  /**
   * Retrieve the [[BlockPos]] dimensions of the schematic for a given gate type.
   *
   * @param tpe The gate type whose dimensions are requested.
   * @return The [[BlockPos]] of the schematic's bounding box.
   */
  def getDimensions(tpe: GateType): BlockPos < SchematicGeneration =
    getSchematic(tpe).map(_.dimensions)

  /**
   * Choose a block from the context palette using the horizontal pin column index.
   *
   * When there are more pins than palette entries the list wraps around.
   * If the palette is somehow empty the head of [[Palette.default]] is returned.
   *
   * @param pin The horizontal pin column index used to select a colour.
   * @return The palette [[Block]] for column `pin`.
   */
  def getPaletteBlock(pin: PinX): Block < SchematicGeneration =
    Env.use(ctx =>
      ctx.palette match
        case Chunk() => Palette.default.head
        case palette => palette((pin.value % palette.size).toInt)
    )

  /**
   * Read the repeater delay setting from the ambient [[SchematicContext]].
   *
   * @return The [[RepeaterDelay]] value within a [[SchematicGeneration]] computation.
   */
  def getRepeaterDelay: RepeaterDelay < SchematicGeneration =
    Env.use(_.repeaterDelay)
