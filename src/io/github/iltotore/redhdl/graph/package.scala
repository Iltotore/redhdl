package io.github.iltotore.redhdl.graph

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.Positive0

type NodeId = NodeId.T
object NodeId extends RefinedType[Int, Positive0]