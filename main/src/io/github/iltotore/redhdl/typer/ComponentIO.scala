package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Type
import kyo.Chunk

/**
 * The input/output type signature of a RedHDL component.
 *
 * [[ComponentIO]] records the named input and output ports of a component together
 * with their [[io.github.iltotore.redhdl.ast.Type]]s.  It is used during type
 * checking to validate expression types against port expectations, and during
 * sub-component resolution to determine which ports of an instance are readable
 * (outputs) and which are writable (inputs).
 *
 * @param inputs  A map from input port [[Identifier]] to its [[Type]].
 * @param outputs A map from output port [[Identifier]] to its [[Type]].
 */
case class ComponentIO(
    inputs: Map[Identifier, Type],
    outputs: Map[Identifier, Type]
)
