package io.github.iltotore.redhdl.graph

import kyo.Maybe
import scala.math.Ordering.Implicits.infixOrderingOps

case class Net(start: PinX, end: PinX):

  val left: PinX = if start < end then start else end
  val right: PinX = if end > start then end else start