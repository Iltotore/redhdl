package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Type
import kyo.Chunk

case class ComponentIO(
    inputs: Map[Identifier, Type],
    outputs: Map[Identifier, Type]
)
