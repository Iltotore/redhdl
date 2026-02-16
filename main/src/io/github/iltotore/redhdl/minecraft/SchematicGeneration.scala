package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.Compilation
import kyo.*

type SchematicGeneration = Env[SchematicContext] & Abort[SchematicFailure]

object SchematicGeneration:

  def run[A, S](context: SchematicContext)(body: A < (SchematicGeneration & S)): A < (Compilation & S) =
    body.handle(
      Env.run(context),
      Abort.recover(Compilation.emitAndAbort)
    )
