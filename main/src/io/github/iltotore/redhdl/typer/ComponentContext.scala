package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type
import kyo.*

/**
 * The mutable type-checking state for a single component body.
 *
 * [[ComponentContext]] is threaded through the body-checking pass of the type
 * checker via the [[Typing.Component]] effect (a [[kyo.Var]]).  It stores the I/O
 * signature of the component being checked, the I/O signatures of its sub-component
 * instances, and the set of ports that still need to be assigned before the body ends.
 *
 * @param currentComponent The I/O signature of the component currently being checked.
 * @param subcomponents    A map from local sub-component instance name to its I/O
 *                         signature, used to resolve [[PortIdentifier.Sub]] references.
 * @param remainingPorts   The set of [[PortIdentifier]]s that must still be assigned
 *                         by the body; updated as assignments are encountered.
 */
case class ComponentContext(
    currentComponent: ComponentIO,
    subcomponents: Map[Identifier, ComponentIO],
    remainingPorts: Set[PortIdentifier]
):

  /**
   * Look up the [[Type]] of a port that may be ''read'' (i.e. used as a value) in
   * an expression.
   *
   * For a [[PortIdentifier.Main]] reference the lookup is performed in the current
   * component's input map.  For a [[PortIdentifier.Sub]] reference it is performed
   * in the named sub-component's output map (reading a sub-component output).
   *
   * @param identifier The port identifier to resolve.
   * @return `Some(type)` if the port exists and is callable, `None` otherwise.
   */
  def getCallableType(identifier: PortIdentifier): Option[Type] = identifier match
    case PortIdentifier.Main(name) => currentComponent.inputs.get(name)
    case PortIdentifier.Sub(subComponent, name) =>
      subcomponents.get(subComponent).flatMap(_.outputs.get(name))

  /**
   * Look up the [[Type]] of a port that may be ''written'' (i.e. assigned a value)
   * in the body.
   *
   * For a [[PortIdentifier.Main]] reference the lookup is performed in the current
   * component's output map.  For a [[PortIdentifier.Sub]] reference it is performed
   * in the named sub-component's input map (driving a sub-component input).
   *
   * @param identifier The port identifier to resolve.
   * @return `Some(type)` if the port exists and is assignable, `None` otherwise.
   */
  def getAssignableType(identifier: PortIdentifier): Option[Type] = identifier match
    case PortIdentifier.Main(name) => currentComponent.outputs.get(name)
    case PortIdentifier.Sub(subComponent, name) =>
      subcomponents.get(subComponent).flatMap(_.inputs.get(name))

/**
 * Factory and stateful accessor helpers for [[ComponentContext]] within the
 * [[Typing.Component]] effect.
 */
object ComponentContext:

  /**
   * Create a minimal [[ComponentContext]] for a component that has no sub-components
   * and whose remaining-port set is empty.
   *
   * @param currentComponent The I/O signature of the component to check.
   * @return A default context with no sub-components and an empty remaining-port set.
   */
  def default(currentComponent: ComponentIO): ComponentContext =
    ComponentContext(currentComponent, Map.empty, Set.empty)

  /**
   * Read the callable [[Type]] of a port from the ambient [[ComponentContext]] state.
   *
   * @param identifier The port to resolve.
   * @return `Some(type)` if callable, `None` otherwise, within [[Typing.Component]].
   */
  def getCallableType(identifier: PortIdentifier): Option[Type] < Typing.Component =
    Var.use(_.getCallableType(identifier))

  /**
   * Read the assignable [[Type]] of a port from the ambient [[ComponentContext]] state.
   *
   * @param identifier The port to resolve.
   * @return `Some(type)` if assignable, `None` otherwise, within [[Typing.Component]].
   */
  def getAssignableType(identifier: PortIdentifier): Option[Type] < Typing.Component =
    Var.use(_.getAssignableType(identifier))
