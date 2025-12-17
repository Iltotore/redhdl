package io.github.iltotore.redhdl.typer

import kyo.*
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type

case class ComponentContext(
  currentComponent: ComponentIO,
  subcomponents: Map[Identifier, ComponentIO],
  remainingPorts: Set[PortIdentifier]
):

  def getCallableType(identifier: PortIdentifier): Option[Type] = identifier match
    case PortIdentifier.Main(name) => currentComponent.inputs.get(name)
    case PortIdentifier.Sub(subComponent, name) =>
      subcomponents.get(subComponent).flatMap(_.outputs.get(name))

  def getAssignableType(identifier: PortIdentifier): Option[Type] = identifier match
    case PortIdentifier.Main(name) => currentComponent.outputs.get(name)
    case PortIdentifier.Sub(subComponent, name) =>
      subcomponents.get(subComponent).flatMap(_.inputs.get(name))
  

object ComponentContext:
  def default(currentComponent: ComponentIO): ComponentContext =
    ComponentContext(currentComponent, Map.empty, Set.empty)

  def getCallableType(identifier: PortIdentifier): Option[Type] < Typing.Component =
    Var.use(_.getCallableType(identifier))

  def getAssignableType(identifier: PortIdentifier): Option[Type] < Typing.Component =
    Var.use(_.getAssignableType(identifier))