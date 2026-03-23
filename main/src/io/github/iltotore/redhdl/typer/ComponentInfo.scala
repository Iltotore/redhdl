package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import kyo.Chunk

/**
 * All type-checked information about a single RedHDL component.
 *
 * [[ComponentInfo]] is the output of the type-checking pass for one component and
 * the input consumed by the IR expansion phase.  It bundles:
 *   - The validated I/O signature ([[io]]).
 *   - The list of sub-component instantiations (name → component type name).
 *   - The raw AST body (port assignments), preserved verbatim after type checking.
 *
 * @param io            The validated input/output signature of the component.
 * @param subcomponents An ordered list of sub-component instantiations, each entry
 *                      mapping a local instance name to the referenced component type
 *                      name.
 * @param body          The body of the component as a sequence of
 *                      `(PortIdentifier, Expr)` assignment pairs, in source order.
 */
case class ComponentInfo(
    io: ComponentIO,
    subcomponents: Chunk[(Identifier, Identifier)],
    body: Chunk[(PortIdentifier, Expr)]
)
