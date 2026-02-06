package io.github.iltotore.redhdl

import kyo.*
import io.github.iltotore.redhdl.ast.Identifier

case class CompilationContext(
  fileName: Maybe[String],
  entrypoint: Maybe[Identifier],
  optimize: Boolean
)

object CompilationContext:

  def fileName: Maybe[String] < Compilation = Env.use(_.fileName)

  def entrypoint: Maybe[Identifier] < Compilation = Env.use(_.entrypoint)

  def optimize: Boolean < Compilation = Env.use(_.optimize)