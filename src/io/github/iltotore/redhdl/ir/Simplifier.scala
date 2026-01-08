package io.github.iltotore.redhdl.ir

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.PortIdentifier
import kyo.Chunk
import io.github.iltotore.redhdl.ir.ExpandedComponent
import io.github.iltotore.redhdl.ir.SimplifiedComponent

object Simplifier:

  def simplifyComponent(component: ExpandedComponent): SimplifiedComponent =
    val simplifiedOutputs = component.io.outputs.map((out, _) =>
      val expr = component.body.collectFirst { case (i, expr) if i == out => expr }
        .getOrElse(throw AssertionError(s"Unassigned output: $out"))

      (out, simplifyExpr(component, expr))
    )

    SimplifiedComponent(Chunk.from(component.io.inputs.keys), simplifiedOutputs)

  def optimizeSimplified(expr: Expr): Expr = expr match
    case Expr.Not(Expr.Not(expr)) => expr
    case Expr.And(left, Expr.LBool(true)) => left
    case Expr.And(Expr.LBool(true), right) => right
    case Expr.And(_, Expr.LBool(false)) => Expr.LBool(false)
    case Expr.And(Expr.LBool(false), _) => Expr.LBool(false)
    case Expr.Or(left, Expr.LBool(false)) => left
    case Expr.Or(Expr.LBool(false), right) => right
    case Expr.Or(_, Expr.LBool(true)) => Expr.LBool(true)
    case Expr.Or(Expr.LBool(true), _) => Expr.LBool(true)
    case Expr.Xor(left, Expr.LBool(false)) => left
    case Expr.Xor(Expr.LBool(false), right) => right
    case _ => expr 
  
  def simplifyExpr(component: ExpandedComponent, expr: Expr): Expr =
    val simplified = expr match
      case Expr.LBool(value) => Expr.LBool(value)
      case Expr.InputCall(identifier) => identifier match
        case PortIdentifier.Sub(subComponent, name) => throw AssertionError(s"Subcomponent port in expanded component: $subComponent.$name")
        case PortIdentifier.Main(name) =>
          if component.io.inputs.contains(name) then Expr.InputCall(PortIdentifier.Main(name))
          else simplifyExpr(component, component.getExpr(name))
      
      case Expr.Not(expr) => Expr.Not(simplifyExpr(component, expr))
      case Expr.Or(left, right) => Expr.Or(simplifyExpr(component, left), simplifyExpr(component, right))
      case Expr.And(left, right) => Expr.And(simplifyExpr(component, left), simplifyExpr(component, right))
      case Expr.Xor(left, right) => Expr.Xor(simplifyExpr(component, left), simplifyExpr(component, right))

    optimizeSimplified(simplified)
  