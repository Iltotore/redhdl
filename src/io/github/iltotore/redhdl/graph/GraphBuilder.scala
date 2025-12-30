package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.graph.Graph
import kyo.*

object GraphBuilder:

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

  def buildOutputsGraph(outputs: Map[Identifier, Expr]): Unit < GraphBuilding =
    Kyo.foreachDiscard(outputs)((output, expr) =>
      Graph.addNode(Node(NodeType.Output(output), Chunk.empty))
        .map(id => buildExprGraph(NodeOutput(id, 0), expr))
    )


