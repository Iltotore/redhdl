package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import kyo.Chunk

case class ComponentInfo(
    io: ComponentIO,
    subcomponents: Chunk[(Identifier, Identifier)],
    body: Chunk[(PortIdentifier, Expr)]
)
