package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.Interval

type RepeaterDelay = RepeaterDelay.T
object RepeaterDelay extends RefinedType[Int, Interval.Closed[1, 4]]
