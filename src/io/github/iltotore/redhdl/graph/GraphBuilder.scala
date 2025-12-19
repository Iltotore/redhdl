package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.graph.Graph.addOutput
import io.github.iltotore.redhdl.graph.Graph.createOutput
import io.github.iltotore.redhdl.graph.Graph.getInputId
import kyo.*

/*
out = a or (b and c)

=>

a ----->or-->out
        ^
b --v   |
    and-+
c --^
 */

object GraphBuilder:

  // For some reason, Kyo-direct translates to a wrong AST
  def buildExprGraph(output: NodeId, expr: Expr): Unit < GraphBuilding =
    expr match
      case Expr.LBool(true)  => createOutput(output, NodeType.True).unit
      case Expr.LBool(false) => createOutput(output, NodeType.False).unit
      case Expr.InputCall(identifier) =>
        getInputId(identifier.asMain).map(addOutput(_, output))
      case Expr.Not(expr) =>
        createOutput(output, NodeType.Not).map(buildExprGraph(_, expr))
      case Expr.Or(left, right) =>
        for
          id <- createOutput(output, NodeType.Or)
          _ <- buildExprGraph(id, left)
          _ <- buildExprGraph(id, right)
        yield ()
      case Expr.And(left, right) =>
        for
          id <- createOutput(output, NodeType.And)
          _ <- buildExprGraph(id, left)
          _ <- buildExprGraph(id, right)
        yield ()

  def buildOutputsGraph(outputs: Map[Identifier, Expr]): Unit < GraphBuilding =
    Kyo.foreachDiscard(outputs)((output, expr) =>
      Graph.addNode(Node(NodeType.Output(output), Chunk.empty)).map(buildExprGraph(_, expr))
    )
