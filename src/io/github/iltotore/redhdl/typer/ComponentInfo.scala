package io.github.iltotore.redhdl.typer

import kyo.Chunk
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier

case class ComponentInfo(
  io: ComponentIO,
  subcomponents: Chunk[(Identifier, Identifier)],
  body: Chunk[(PortIdentifier, Expr)]
)