package io.github.iltotore.redhdl.ast.untpd

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type
import kyo.Chunk

case class Component(
    name: Identifier,
    inputs: Chunk[(Identifier, Type)],
    outputs: Chunk[(Identifier, Type)],
    body: Chunk[(PortIdentifier, Expr)]
)
