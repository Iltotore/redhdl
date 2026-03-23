package io.github.iltotore.redhdl.graph

import kyo.Maybe
import scala.math.Ordering.Implicits.infixOrderingOps

/*
Net(0, 1)
Net(2, 0)
Net(3, 4)

Track1: Net(0, 1), Net(3, 4)
Track2: Net(2, 0)

0 1 2 3 4...
|   | |
+   | +
--- | ---
  + |   +
  | |   |
  | +   |
-----   |
+ |     |
| |     |
0 1 2 3 4...

 */

/**
 * A single wire segment in a routing [[Channel]].
 *
 * A net represents a connection between a node output column (`start`) and a node
 * input column (`end`).  The [[left]] and [[right]] convenience fields always give
 * the smaller and larger X coordinate respectively, regardless of signal direction.
 *
 * When cycle-breaking forces a net to be re-routed through an outer column, the
 * original net is truncated and `outerNet` points to its continuation.
 *
 * @param start    The X column at which the net originates (output side).
 * @param end      The X column at which the net terminates (input side).
 * @param outerNet When present, the [[NetId]] of the continuation net that carries
 *                 the signal through the outer column region.
 */
case class Net(start: PinX, end: PinX, outerNet: Maybe[NetId]):

  /** The leftmost column of this net (minimum of `start` and `end`). */
  val left: PinX = if start < end then start else end

  /** The rightmost column of this net (maximum of `start` and `end`). */
  val right: PinX = if end > start then end else start

  /**
   * Whether the net occupies a single column (i.e. `start == end`).
   *
   * Single-line nets are drawn as a vertical wire segment without a horizontal bridge.
   *
   * @return `true` if the net begins and ends at the same X position.
   */
  def isSingleLine: Boolean = start == end
