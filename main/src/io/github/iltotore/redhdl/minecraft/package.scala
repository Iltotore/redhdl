package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.Interval

/**
 * A refined [[Int]] type representing the delay ticks of a Minecraft redstone repeater.
 *
 * Valid values are in the closed interval [1, 4]:
 *   - `1` is the fastest setting (1 game tick delay).
 *   - `4` is the slowest setting (4 game tick delay).
 *
 * The refinement is enforced by the [[io.github.iltotore.iron]] library.  Use
 * `RepeaterDelay(n)` for compile-time-checked construction and
 * `RepeaterDelay.assume(n)` in contexts where the invariant is guaranteed at runtime.
 */
type RepeaterDelay = RepeaterDelay.T

/**
 * Companion object for the [[RepeaterDelay]] refined type.
 *
 * Extends [[io.github.iltotore.iron.RefinedType]] to provide smart constructors and
 * the standard Iron utilities (`assume`, `assumeAll`, etc.).
 */
object RepeaterDelay extends RefinedType[Int, Interval.Closed[1, 4]]
