package io.github.iltotore.redhdl.graph

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.Positive0

type NodeId = NodeId.T
object NodeId extends RefinedType[Int, Positive0]

type NetId = NetId.T
object NetId extends RefinedType[Int, Positive0]

type TrackId = TrackId.T
object TrackId extends RefinedType[Int, Positive0]

type PinX = PinX.T
object PinX extends RefinedType[Int, Positive0]:

  given Ordering[PinX] = PinX.assumeAll(Ordering.Int)