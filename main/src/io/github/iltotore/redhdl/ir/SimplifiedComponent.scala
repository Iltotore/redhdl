package io.github.iltotore.redhdl.ir

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.typer.ComponentIO
import kyo.Chunk

/**
 * The final intermediate representation of a component, ready for graph construction.
 *
 * A [[SimplifiedComponent]] is produced by [[Simplifier.simplifyComponent]] from an
 * [[ExpandedComponent]].  At this stage:
 *   - All sub-component instantiations have been inlined (no `Sub` port references remain).
 *   - Internal port assignments have been substituted away.
 *   - Optional constant-folding and algebraic simplifications have been applied.
 *
 * The result is a flat mapping from each output port to the minimal [[Expr]] tree
 * that computes its value in terms of the top-level input ports only.  This form is
 * consumed directly by [[io.github.iltotore.redhdl.graph.GraphBuilding.buildGraph]].
 *
 * @param inputs  The ordered list of top-level input port identifiers.
 * @param outputs A map from each output port [[Identifier]] to the simplified [[Expr]]
 *                that drives it.
 */
case class SimplifiedComponent(inputs: Chunk[Identifier], outputs: Map[Identifier, Expr])
