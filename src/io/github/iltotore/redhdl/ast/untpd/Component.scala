package io.github.iltotore.redhdl.ast.untpd

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Type

case class Component(
    name: Identifier,
    inputs: List[(Identifier, Type)],
    outputs: List[(Identifier, Type)],
    body: List[Expr]
)
