package io.github.iltotore.redhdl.typer

import kyo.*
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type

case class GlobalContext(
  components: Map[Identifier, ComponentIO]
):

  def getComponent(name: Identifier): Option[ComponentIO] =
    components.get(name)

  def declareComponent(name: Identifier, io: ComponentIO): GlobalContext =
    this.copy(components = components.updated(name, io))

object GlobalContext:
  val default: GlobalContext = GlobalContext(Map(
    Identifier("Not") -> ComponentIO(
      inputs = Map(Identifier("in") -> Type.Bool),
      outputs = Map(Identifier("out") -> Type.Bool)
    ),
    Identifier("Or") -> ComponentIO(
      inputs = Map(
        Identifier("inA") -> Type.Bool,
        Identifier("inB") -> Type.Bool
      ),
      outputs = Map(Identifier("out") -> Type.Bool)
    ),
  ))
  
  def getComponent(name: Identifier): Option[ComponentIO] < Typing.Global =
    Var.use[GlobalContext](_.getComponent(name))

  def declareComponent(name: Identifier, io: ComponentIO): Unit < Typing.Global =
    Var.use[GlobalContext](_.declareComponent(name, io))
      .map(Var.setDiscard)