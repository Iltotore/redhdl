package io.github.iltotore.redhdl.minecraft

import kyo.*

type SchematicGeneration = Env[SchematicContext] & Abort[SchematicFailure]

object SchematicGeneration:

  def run[A, S](context: SchematicContext)(body: A < (SchematicGeneration & S)): Result[SchematicFailure, A] < S =
    body.handle(
      Env.run(context),
      Abort.run
    )