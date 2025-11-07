package io.github.iltotore.redhdl.ast

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type
import kyo.Chunk
import io.github.iltotore.redhdl.ast.Expr

case class Component(
    name: Identifier,
    inputs: Chunk[(Identifier, Type)],
    outputs: Chunk[(Identifier, Type)],
    subcomponents: Chunk[(Identifier, Identifier)],
    body: Chunk[(PortIdentifier, Expr)]
)
