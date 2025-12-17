package io.github.iltotore.redhdl.ast

import kyo.Chunk
import io.github.iltotore.redhdl.ast.Component

case class Program(components: Chunk[Component])