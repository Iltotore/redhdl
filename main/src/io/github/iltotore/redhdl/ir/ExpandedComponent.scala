package io.github.iltotore.redhdl.ir

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Type
import io.github.iltotore.redhdl.typer.ComponentIO
import kyo.Chunk

/**
 * An intermediate representation (IR) of a component after sub-component instantiations
 * have been inlined.
 *
 * An [[ExpandedComponent]] is produced by [[Expander.expandComponent]].  All
 * [[io.github.iltotore.redhdl.ast.PortIdentifier.Sub]] references in the body have
 * been replaced with fresh, prefixed [[io.github.iltotore.redhdl.ast.PortIdentifier.Main]]
 * names of the form `<instance>\$<port>`.  Internal wires introduced by the expansion
 * are recorded in [[internalPorts]].
 *
 * After expansion the component can be simplified by [[Simplifier.simplifyComponent]],
 * which inlines the internal-port assignments and produces a [[SimplifiedComponent]].
 *
 * @param io            The original I/O signature (inputs and outputs) of the component.
 * @param internalPorts Ports generated during expansion to represent sub-component
 *                      I/O signals; not part of the public interface.
 * @param body          The flattened port-assignment list.  Every left-hand side is now
 *                      a simple (un-prefixed) [[Identifier]].
 */
case class ExpandedComponent(
    io: ComponentIO,
    internalPorts: Chunk[(Identifier, Type)],
    body: Chunk[(Identifier, Expr)]
):

  /**
   * Look up the [[Expr]] assigned to a given internal or output port.
   *
   * @param port The port [[Identifier]] to look up.
   * @return The [[Expr]] that drives `port`.
   * @throws AssertionError if `port` is not present in the body, indicating a
   *                        compiler invariant violation.
   */
  def getExpr(port: Identifier): Expr =
    body.find(_._1 == port).fold(throw AssertionError(s"Expanded $port not found"))(_._2)
