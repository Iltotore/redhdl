package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.Compilation
import kyo.*

/**
 * The composite Kyo effect type for the schematic generation phase.
 *
 * A computation of type `A < SchematicGeneration` may:
 *   - Read the ambient [[SchematicContext]] (loaded gate schematics, palette, delay)
 *     via [[kyo.Env]].
 *   - Abort with a [[SchematicFailure]] if a required resource is missing or invalid
 *     via [[kyo.Abort]].
 *
 * [[SchematicGeneration]] is a sub-effect of [[Compilation]]; use [[SchematicGeneration.run]]
 * to fold it back into the broader [[Compilation]] effect context.
 */
type SchematicGeneration = Env[SchematicContext] & Abort[SchematicFailure]

/**
 * Runner for [[SchematicGeneration]] computations.
 */
object SchematicGeneration:

  /**
   * Execute a [[SchematicGeneration]] computation inside an existing [[Compilation]]
   * context.
   *
   * The provided [[SchematicContext]] is injected into the environment and any
   * [[SchematicFailure]] abort is converted to a [[Compilation]] abort by forwarding
   * the failure through [[io.github.iltotore.redhdl.Compilation.emitAndAbort]].
   *
   * @tparam A The value type produced by the computation.
   * @tparam S Additional effects carried by the computation.
   * @param context The [[SchematicContext]] to make available to `body`.
   * @param body    The schematic-generation computation to execute.
   * @return The result of `body` with [[SchematicGeneration]] effects eliminated,
   *         within the remaining [[Compilation]] effect context.
   */
  def run[A, S](context: SchematicContext)(body: A < (SchematicGeneration & S)): A < (Compilation & S) =
    body.handle(
      Env.run(context),
      Abort.recover(Compilation.emitAndAbort)
    )
