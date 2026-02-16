package io.github.iltotore.redhdl.ast

import io.github.iltotore.redhdl.ast.Component
import kyo.Chunk

case class Program(components: Chunk[Component])
