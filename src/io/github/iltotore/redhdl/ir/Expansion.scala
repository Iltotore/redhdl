package io.github.iltotore.redhdl.ir

import kyo.*
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.typer.ComponentInfo

type Expansion = Env[Map[Identifier, ComponentInfo]]

object Expansion:

  def run[A, S](env: Map[Identifier, ComponentInfo])(body: A < (Expansion & S)): A < S =
    Env.run(env)(body)

  def getComponent(identifier: Identifier): ComponentInfo < Expansion =
    Env.use[Map[Identifier, ComponentInfo]](_(identifier))