package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.PortIdentifier
import kyo.Chunk

object Simplifier:

  def simplifyComponent(component: ExpandedComponent): SimplifiedComponent =
    val simplifiedOutputs = component.io.outputs.map((out, _) =>
      val expr = component.body.collectFirst { case (i, expr) if i == out => expr }
        .getOrElse(throw AssertionError(s"Unassigned output: $out"))

      (out, simplifyExpr(component, expr))
    )

    SimplifiedComponent(Chunk.from(component.io.inputs.keys), simplifiedOutputs)

  def simplifyExpr(component: ExpandedComponent, expr: Expr): Expr = expr match
    case Expr.LBool(value) => Expr.LBool(value)
    case Expr.InputCall(identifier) => identifier match
      case PortIdentifier.Sub(subComponent, name) => throw AssertionError(s"Subcomponent port in expanded component: $subComponent.$name")
      case PortIdentifier.Main(name) =>
        if component.io.inputs.contains(name) then Expr.InputCall(PortIdentifier.Main(name))
        else simplifyExpr(component, component.getExpr(name))
    
    case Expr.Not(expr) => Expr.Not(simplifyExpr(component, expr))
    case Expr.Or(left, right) => Expr.Or(simplifyExpr(component, left), simplifyExpr(component, right))
    case Expr.And(left, right) => Expr.And(simplifyExpr(component, left), simplifyExpr(component, right))
  