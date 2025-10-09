package io.github.iltotore.redhdl.ast

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type Identifier = Identifier.T
object Identifier extends RefinedType[String, Not[Blank]]
