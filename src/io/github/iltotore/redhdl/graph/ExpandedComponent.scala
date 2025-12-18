package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Type
import io.github.iltotore.redhdl.typer.ComponentIO
import kyo.Chunk

case class ExpandedComponent(
  io: ComponentIO,
  internalPorts: Chunk[(Identifier, Type)],
  body: Chunk[(Identifier, Expr)]
)
