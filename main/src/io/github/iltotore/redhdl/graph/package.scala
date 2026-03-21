package io.github.iltotore.redhdl.graph

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.Interval
import io.github.iltotore.iron.constraint.numeric.Positive0
import scala.annotation.targetName

/**
 * A refined [[Int]] type representing the zero-based index of a [[Node]] in a
 * [[Graph]].
 *
 * The `Positive0` constraint ensures the value is non-negative.
 */
type NodeId = NodeId.T

/**
 * Companion object for the [[NodeId]] refined type.
 *
 * Provides smart constructors (`NodeId(n)`, `NodeId.assume(n)`) and the standard
 * Iron utilities.
 */
object NodeId extends RefinedType[Int, Positive0]

/**
 * A refined [[Int]] type representing the zero-based index of a [[Net]] in a
 * [[Channel]].
 *
 * The `Positive0` constraint ensures the value is non-negative.  An [[Integral]]
 * instance is provided so that [[NetId]] values can participate in arithmetic
 * expressions (e.g. `Chunk.range`) without manual unwrapping.
 */
type NetId = NetId.T

/**
 * Companion object for the [[NetId]] refined type.
 *
 * Exposes an [[scala.math.Integral]] instance delegating to [[scala.math.Numeric.IntIsIntegral]]
 * so that ranges and arithmetic work uniformly on [[NetId]] values.
 */
object NetId extends RefinedType[Int, Positive0]:

  /** Integral instance for [[NetId]], enabling `Chunk.range` and integer arithmetic. */
  given Integral[NetId] = NetId.assumeAll(Numeric.IntIsIntegral)

/**
 * A refined [[Int]] type representing the zero-based index of a [[Track]] in a
 * [[Channel]].
 *
 * The `Positive0` constraint ensures the value is non-negative.
 */
type TrackId = TrackId.T

/**
 * Companion object for the [[TrackId]] refined type.
 */
object TrackId extends RefinedType[Int, Positive0]

/**
 * A refined [[Int]] type representing a horizontal pin column in the schematic layout.
 *
 * The `Positive0` constraint ensures the value is non-negative.  Ordering and equality
 * instances are provided so that [[PinX]] values can be compared and sorted without
 * unwrapping.  An extension method adds integer offsets directly to [[PinX]] values.
 */
type PinX = PinX.T

/**
 * Companion object for the [[PinX]] refined type.
 *
 * Provides [[CanEqual]] and [[scala.math.Ordering]] instances so that [[PinX]] values
 * support `==`, `<`, `>`, etc.  The `+` extension method allows adding a plain `Int`
 * offset to a [[PinX]] while preserving the refinement.
 */
object PinX extends RefinedType[Int, Positive0]:

  /** Enables structural equality between two [[PinX]] values. */
  given CanEqual[PinX, PinX] = CanEqual.derived

  /** Total ordering on [[PinX]] values, delegating to integer ordering. */
  given Ordering[PinX] = PinX.assumeAll(Ordering.Int)

  extension (x: PinX)
    /**
     * Add an integer offset to this [[PinX]], returning a new [[PinX]].
     *
     * @param y The integer amount to add.
     * @return A new [[PinX]] equal to `x.value + y`.
     */
    def +(y: Int): PinX = PinX.assume(x.value + y)

/**
 * A refined [[Int]] type for the number of inputs of an OR gate.
 *
 * Valid values are in the closed interval [2, 6], matching the available
 * `or_2` through `or_6` gate schematics.
 */
type OrSize = OrSize.T

/**
 * Companion object for the [[OrSize]] refined type.
 *
 * Exposes minimum and maximum constants, an [[scala.math.Integral]] instance, and an
 * extension method for incrementing an [[OrSize]] by an integer.
 */
object OrSize extends RefinedType[Int, Interval.Closed[2, 6]]:
  /** The smallest valid OR gate size (2 inputs). */
  val MinValue: OrSize = OrSize(2)

  /** The largest valid OR gate size (6 inputs). */
  val MaxValue: OrSize = OrSize(6)

  /** Integral instance for [[OrSize]], enabling range iteration. */
  given Integral[OrSize] = OrSize.assumeAll(Numeric.IntIsIntegral)

  extension (x: OrSize)
    /**
     * Add an integer offset to this [[OrSize]], returning a new [[OrSize]].
     *
     * @param y The integer amount to add.
     * @return A new [[OrSize]] equal to `x.value + y`.
     */
    def +(y: Int): OrSize = OrSize.assume(x.value + y)

/**
 * A refined [[Int]] type for the number of inputs of an AND gate.
 *
 * Valid values are in the closed interval [2, 8], matching the available
 * `and_2` through `and_8` gate schematics.
 */
type AndSize = AndSize.T

/**
 * Companion object for the [[AndSize]] refined type.
 *
 * Exposes minimum and maximum constants, an [[scala.math.Integral]] instance, and an
 * extension method for incrementing an [[AndSize]] by an integer.
 */
object AndSize extends RefinedType[Int, Interval.Closed[2, 8]]:
  /** The smallest valid AND gate size (2 inputs). */
  val MinValue: AndSize = AndSize(2)

  /** The largest valid AND gate size (8 inputs). */
  val MaxValue: AndSize = AndSize(8)

  /** Integral instance for [[AndSize]], enabling range iteration. */
  given Integral[AndSize] = AndSize.assumeAll(Numeric.IntIsIntegral)

  extension (x: AndSize)
    /**
     * Add an integer offset to this [[AndSize]], returning a new [[AndSize]].
     *
     * @param y The integer amount to add.
     * @return A new [[AndSize]] equal to `x.value + y`.
     */
    def +(y: Int): AndSize = AndSize.assume(x.value + y)
