package io.github.iltotore.redhdl.ir

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type
import io.github.iltotore.redhdl.typer.ComponentInfo
import kyo.*
import io.github.iltotore.redhdl.ir.ExpandedComponent
import io.github.iltotore.redhdl.ir.Expansion

object Expander:

  private def prefixIdentifier(prefix: Identifier, suffix: Identifier): Identifier =
    Identifier.assume(s"$prefix$$$suffix")

  def expandExpr(expr: Expr): Expr = expr match
    case Expr.LBool(value) => Expr.LBool(value)
    case Expr.InputCall(identifier) =>
      identifier match
        case PortIdentifier.Main(name) =>
          Expr.InputCall(PortIdentifier.Main(name))
        case PortIdentifier.Sub(subComponent, name) =>
          Expr.InputCall(PortIdentifier.Main(prefixIdentifier(subComponent, name)))
      
    case Expr.Not(expr) => Expr.Not(expandExpr(expr))
    case Expr.Or(left, right) => Expr.Or(expandExpr(left), expandExpr(right))
    case Expr.And(left, right) => Expr.And(expandExpr(left), expandExpr(right))
  
  def prefixExpr(prefix: Identifier, expr: Expr): Expr = expr match
    case Expr.LBool(value) => Expr.LBool(value)
    case Expr.InputCall(identifier) =>
      identifier match
        case PortIdentifier.Main(name) => Expr.InputCall(PortIdentifier.Main(prefixIdentifier(prefix, name)))
        case PortIdentifier.Sub(subComponent, name) => throw AssertionError(s"Subcomponent port in expanded component: $subComponent.$name")
    case Expr.Not(expr) => Expr.Not(prefixExpr(prefix, expr))
    case Expr.Or(left, right) => Expr.Or(prefixExpr(prefix, left), prefixExpr(prefix, right))
    case Expr.And(left, right) => Expr.And(prefixExpr(prefix, left), prefixExpr(prefix, right))

  def expandComponent(component: ComponentInfo): ExpandedComponent < Expansion = direct:
    val flattenedBody = component.body.map:
      case (PortIdentifier.Main(name), expr)     => (name, expandExpr(expr))
      case (PortIdentifier.Sub(sub, name), expr) => (prefixIdentifier(sub, name), expandExpr(expr))

    val (internalPorts, expandedBody) = component.subcomponents
      .foldLeft((Chunk.empty[(Identifier, Type)], flattenedBody)):
        case ((ports, body), (name, tpe)) =>
          val sub = expandComponent(Expansion.getComponent(tpe).now).now

          val renamedInputs = sub.io.inputs.map((subName, subType) => (prefixIdentifier(name, subName), subType))
          val renamedOutputs = sub.io.outputs.map((subName, subType) => (prefixIdentifier(name, subName), subType))
          val renamedInternals = sub.internalPorts.map((subName, subType) => (prefixIdentifier(name, subName), subType))

          val renamedBody = sub.body.map((subName, expr) => (prefixIdentifier(name, subName), prefixExpr(name, expr)))

          (ports ++ renamedInputs ++ renamedOutputs ++ renamedInternals, body ++ renamedBody)

    ExpandedComponent(
      io = component.io,
      internalPorts = internalPorts,
      body = expandedBody
    )
