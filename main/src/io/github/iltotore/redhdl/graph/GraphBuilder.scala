package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.graph.Graph
import kyo.*

/**
 * Translates a simplified Boolean expression tree into a logic [[Graph]].
 *
 * The builder works in a ''bottom-up'' fashion: given the [[NodeOutput]] slot that
 * needs to be driven, it recursively creates the gate nodes required to compute
 * the expression and wires them together.
 */
object GraphBuilder:

  /**
   * Add the gate nodes for a single [[Expr]] into the current graph so that the
   * result drives the given `output` slot.
   *
   * The method is recursive: compound expressions (NOT, OR, AND, XOR) allocate a
   * gate node for themselves and then recurse on their sub-expressions, passing
   * the gate's input slots as the new `output` targets.
   *
   * @note Kyo-direct syntax produces a wrong AST for this particular definition,
   *       so it is written with explicit for-comprehensions and `.map` instead.
   *
   * @param output The [[NodeOutput]] slot that the top-level result should be wired to.
   * @param expr   The expression to compile into graph nodes.
   */
  // For some reason, Kyo-direct translates to a wrong AST here.
  def buildExprGraph(output: NodeOutput, expr: Expr): Unit < GraphBuilding =
    expr match
      case Expr.LBool(true)  => Graph.createOutput(output, NodeType.True).unit
      case Expr.LBool(false) => Graph.createOutput(output, NodeType.False).unit
      case Expr.InputCall(identifier) =>
        Graph.getInputId(identifier.asMain).map(Graph.addOutput(_, output))
      case Expr.Not(expr) =>
        Graph.createOutput(output, NodeType.Not).map(id => buildExprGraph(NodeOutput(id, 0), expr))
      case Expr.Or(left, right) =>
        for
          id <- Graph.createOutput(output, NodeType.Or)
          _ <- buildExprGraph(NodeOutput(id, 0), left)
          _ <- buildExprGraph(NodeOutput(id, 1), right)
        yield ()
      case Expr.And(left, right) =>
        for
          id <- Graph.createOutput(output, NodeType.And)
          _ <- buildExprGraph(NodeOutput(id, 0), left)
          _ <- buildExprGraph(NodeOutput(id, 1), right)
        yield ()
      case Expr.Xor(left, right) =>
        for
          id <- Graph.createOutput(output, NodeType.Xor)
          _ <- buildExprGraph(NodeOutput(id, 0), left)
          _ <- buildExprGraph(NodeOutput(id, 1), right)
        yield ()

  /**
   * Build the output nodes for a simplified component and wire each output port
   * to the expression that drives it.
   *
   * For every `(outputName, expr)` pair an [[NodeType.Output]] node is created and
   * [[buildExprGraph]] is called to construct the driver logic.
   *
   * @param outputs A map from output port [[Identifier]] to the [[Expr]] that
   *                computes its value.
   */
  def buildOutputsGraph(outputs: Map[Identifier, Expr]): Unit < GraphBuilding =
    Kyo.foreachDiscard(outputs)((output, expr) =>
      Graph.addNode(Node(NodeType.Output(output), Chunk.empty))
        .map(id => buildExprGraph(NodeOutput(id, 0), expr))
    )
