package io.github.iltotore.redhdl.ast

/**
 * AST node that uniquely identifies a port within a component.
 *
 * Ports come in two flavours depending on which component they belong to:
 *   - [[PortIdentifier.Main]] – a port declared directly on the current component.
 *   - [[PortIdentifier.Sub]]  – a port belonging to one of the current component's
 *     sub-component instances, addressed by the instance name and the port name.
 */
enum PortIdentifier:
  /**
   * A port on the current (top-level) component.
   *
   * @param name The port name as declared in the `input` or `output` section.
   */
  case Main(name: Identifier)

  /**
   * A port on an instantiated sub-component.
   *
   * @param subComponent The local instance name of the sub-component.
   * @param name         The name of the port on that sub-component.
   */
  case Sub(subComponent: Identifier, name: Identifier)

  /**
   * Unwrap this [[PortIdentifier]] as a simple [[Identifier]], asserting that
   * it is a [[Main]] port.
   *
   * After the IR expansion phase all sub-component ports are inlined, so every
   * remaining [[PortIdentifier]] must be [[Main]].  This helper makes that
   * invariant explicit.
   *
   * @return The [[Identifier]] of the `Main` port.
   * @throws AssertionError if called on a [[Sub]] port, indicating a compiler bug.
   */
  def asMain: Identifier = this match
    case PortIdentifier.Main(name)              => name
    case PortIdentifier.Sub(subComponent, name) => throw AssertionError(s"Subcomponent port in expanded component: $subComponent.$name")
