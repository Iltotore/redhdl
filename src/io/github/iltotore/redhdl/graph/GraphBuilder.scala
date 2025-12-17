package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Expr
import kyo.*

object GraphBuilder:

  def buildExprGraph(expr: Expr): NodeId < GraphBuilding = direct:
    expr match
      case Expr.LBool(true)           => Graph.addNode(Node(NodeType.True)).now
      case Expr.LBool(false)          => Graph.addNode(Node(NodeType.False)).now
      case Expr.InputCall(identifier) => ??? // Graph.addNode(Node(NodeType.Input()))
      case Expr.Not(expr)             => Graph.addNode(Node(NodeType.Not, Chunk(buildExprGraph(expr).now))).now
      case Expr.Or(left, right)       => Graph.addNode(Node(NodeType.Or, Chunk(buildExprGraph(left).now, buildExprGraph(right).now))).now
      case Expr.And(left, right)      => Graph.addNode(Node(NodeType.And, Chunk(buildExprGraph(left).now, buildExprGraph(right).now))).now
