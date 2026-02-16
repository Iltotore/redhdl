package io.github.iltotore.redhdl.ir

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.typer.ComponentIO
import kyo.Chunk

case class SimplifiedComponent(inputs: Chunk[Identifier], outputs: Map[Identifier, Expr])
