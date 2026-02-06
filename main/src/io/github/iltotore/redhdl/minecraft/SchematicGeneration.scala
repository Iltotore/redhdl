package io.github.iltotore.redhdl.minecraft

import kyo.*
import io.github.iltotore.redhdl.Compilation

type SchematicGeneration = Env[SchematicContext] & Abort[SchematicFailure]

object SchematicGeneration:

  def run[A, S](context: SchematicContext)(body: A < (SchematicGeneration & S)): A < (Compilation & S) =
    body.handle(
      Env.run(context),
      Abort.recover(Compilation.emitAndAbort)
    )