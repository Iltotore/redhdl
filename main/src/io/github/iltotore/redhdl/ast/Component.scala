package io.github.iltotore.redhdl.ast

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type
import kyo.Chunk

/**
 * AST node representing a single named hardware component declaration.
 *
 * A component is the top-level unit of a RedHDL program.  It describes a
 * combinational logic block with named input and output ports, optional
 * sub-component instantiations, and a body that assigns an [[Expr]] to
 * every output (and sub-component input) port.
 *
 * @param name          The unique name by which this component is referenced.
 * @param inputs        The input ports declared for this component, each paired
 *                      with its [[Type]].
 * @param outputs       The output ports declared for this component, each paired
 *                      with its [[Type]].
 * @param subcomponents Instantiated sub-components, mapping a local instance name
 *                      to the component type being instantiated.
 * @param body          Port assignments: each entry maps a [[PortIdentifier]] to
 *                      the [[Expr]] that drives it.  A `Main` identifier targets an
 *                      output of this component; a `Sub` identifier targets an input
 *                      of one of the sub-component instances.
 */
case class Component(
    name: Identifier,
    inputs: Chunk[(Identifier, Type)],
    outputs: Chunk[(Identifier, Type)],
    subcomponents: Chunk[(Identifier, Identifier)],
    body: Chunk[(PortIdentifier, Expr)]
)
