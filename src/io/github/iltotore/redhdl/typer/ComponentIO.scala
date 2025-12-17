package io.github.iltotore.redhdl.typer

import kyo.Chunk
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Type

case class ComponentIO(
  inputs: Map[Identifier, Type],
  outputs: Map[Identifier, Type]
)