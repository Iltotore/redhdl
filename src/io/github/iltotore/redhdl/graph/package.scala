package io.github.iltotore.redhdl.graph

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.Interval
import io.github.iltotore.iron.constraint.numeric.Positive0
import scala.annotation.targetName

type NodeId = NodeId.T
object NodeId extends RefinedType[Int, Positive0]

type NetId = NetId.T
object NetId extends RefinedType[Int, Positive0]:

  given Integral[NetId] = NetId.assumeAll(Numeric.IntIsIntegral)

type TrackId = TrackId.T
object TrackId extends RefinedType[Int, Positive0]

type PinX = PinX.T
object PinX extends RefinedType[Int, Positive0]:

  given CanEqual[PinX, PinX] = CanEqual.derived

  given Ordering[PinX] = PinX.assumeAll(Ordering.Int)

  extension (x: PinX)
    def +(y: Int): PinX = PinX.assume(x.value + y)

type OrSize = OrSize.T
object OrSize extends RefinedType[Int, Interval.Closed[2, 6]]:
  val MinValue: OrSize = OrSize(2)
  val MaxValue: OrSize = OrSize(6)

  given Integral[OrSize] = OrSize.assumeAll(Numeric.IntIsIntegral)

  extension (x: OrSize)
    def +(y: Int): OrSize = OrSize.assume(x.value + y)

type AndSize = AndSize.T
object AndSize extends RefinedType[Int, Interval.Closed[2, 8]]:
  val MinValue: AndSize = AndSize(2)
  val MaxValue: AndSize = AndSize(8)

  given Integral[AndSize] = AndSize.assumeAll(Numeric.IntIsIntegral)

  extension (x: AndSize)
    def +(y: Int): AndSize = AndSize.assume(x.value + y)
