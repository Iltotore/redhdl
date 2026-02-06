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

case class Net(start: PinX, end: PinX, outerNet: Maybe[NetId]):

  val left: PinX = if start < end then start else end
  val right: PinX = if end > start then end else start

  def isSingleLine: Boolean = start == end