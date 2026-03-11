package io.github.iltotore.redhdl

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.minecraft.Block
import kyo.*

case class CompilationContext(
    fileName: Maybe[String],
    entrypoint: Maybe[Identifier],
    optimize: Boolean,
    alignOutputs: Boolean,
    palette: Chunk[Block]
)

object CompilationContext:

  def fileName: Maybe[String] < Compilation = Env.use(_.fileName)

  def entrypoint: Maybe[Identifier] < Compilation = Env.use(_.entrypoint)

  def optimize: Boolean < Compilation = Env.use(_.optimize)

  def alignOutputs: Boolean < Compilation = Env.use(_.alignOutputs)

  def palette: Chunk[Block] < Compilation = Env.use(_.palette)
