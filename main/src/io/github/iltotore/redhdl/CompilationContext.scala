package io.github.iltotore.redhdl

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.minecraft.Block
import io.github.iltotore.redhdl.minecraft.RepeaterDelay
import kyo.*

/**
 * Immutable configuration passed through the entire compilation pipeline.
 *
 * An instance is provided to each compilation stage via the [[kyo.Env]] effect layer;
 * use the accessors on [[CompilationContext$]] to read individual fields inside a
 * [[Compilation]] computation.
 *
 * @param fileName      The base name of the source file being compiled, used as the
 *                      default entry-point name when no explicit entrypoint is given.
 * @param entrypoint    An explicitly requested entry-point component name.  When
 *                      [[kyo.Absent]] the compiler falls back to `fileName` and then to
 *                      the component named `Main`.
 * @param optimize      Whether algebraic and constant-folding optimisations are enabled.
 * @param alignOutputs  Whether output nodes should be placed in a dedicated final layer
 *                      so they all appear at the same Z coordinate in the schematic.
 * @param palette       Ordered list of blocks used to colour circuit wires.  The list is
 *                      cycled when there are more wires than colours.
 * @param repeaterDelay Delay ticks configured on every redstone repeater in the generated
 *                      schematic (1 = fastest, 4 = slowest).
 */
case class CompilationContext(
    fileName: Maybe[String],
    entrypoint: Maybe[Identifier],
    optimize: Boolean,
    alignOutputs: Boolean,
    palette: Chunk[Block],
    repeaterDelay: RepeaterDelay
)

/**
 * Convenience accessors for individual [[CompilationContext]] fields within a
 * [[Compilation]] effect.
 */
object CompilationContext:

  /**
   * Read the optional source file name from the ambient [[CompilationContext]].
   *
   * @return The file name wrapped in [[kyo.Maybe]], or [[kyo.Absent]] if not set.
   */
  def fileName: Maybe[String] < Compilation = Env.use(_.fileName)

  /**
   * Read the optional explicit entry-point identifier from the ambient context.
   *
   * @return The entry-point name wrapped in [[kyo.Maybe]], or [[kyo.Absent]] if not set.
   */
  def entrypoint: Maybe[Identifier] < Compilation = Env.use(_.entrypoint)

  /**
   * Read the optimisation flag from the ambient [[CompilationContext]].
   *
   * @return `true` if optimisations are enabled.
   */
  def optimize: Boolean < Compilation = Env.use(_.optimize)

  /**
   * Read the output-alignment flag from the ambient [[CompilationContext]].
   *
   * @return `true` if output-node alignment is enabled.
   */
  def alignOutputs: Boolean < Compilation = Env.use(_.alignOutputs)

  /**
   * Read the wire colour palette from the ambient [[CompilationContext]].
   *
   * @return The [[kyo.Chunk]] of blocks used to colour wires.
   */
  def palette: Chunk[Block] < Compilation = Env.use(_.palette)

  /**
   * Read the repeater delay setting from the ambient [[CompilationContext]].
   *
   * @return The [[RepeaterDelay]] value (1–4 inclusive).
   */
  def repeaterDelay: RepeaterDelay < Compilation = Env.use(_.repeaterDelay)
