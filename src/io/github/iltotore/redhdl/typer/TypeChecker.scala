package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Component
import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type
import kyo.*
import io.github.iltotore.redhdl.ast.Program

object TypeChecker:

  def assertType(tpe: Type, expected: Type): Unit < Typing = direct:
    if tpe == expected then ()
    else Typing.fail(TypeFailure.Mismatch(tpe, expected)).now

  def assertExprType(expr: Expr, expected: Type): Unit < Typing.Component =
    getType(expr).map(assertType(_, expected))

  def getType(expr: Expr): Type < Typing.Component = direct:
    expr match
      case Expr.LBool(_) => Type.Bool
      case Expr.InputCall(identifier) =>
        ComponentContext.getCallableType(identifier).now match
          case Some(tpe) => tpe
          case None      => Typing.failAndAbort(TypeFailure.UnknownCallablePort(identifier)).now
      case Expr.Not(expr) =>
        assertExprType(expr, Type.Bool).now
        Type.Bool
      case Expr.Or(left, right) =>
        assertExprType(left, Type.Bool).now
        assertExprType(right, Type.Bool).now
        Type.Bool
      case Expr.And(left, right) =>
        assertExprType(left, Type.Bool).now
        assertExprType(right, Type.Bool).now
        Type.Bool

  def checkBody(body: Chunk[(PortIdentifier, Expr)]): Unit < Typing.Component = direct:
    body.foreach((name, expr) =>
      val exprType = getType(expr).now
      ComponentContext.getAssignableType(name).now match
        case Some(tpe) => assertType(exprType, tpe).now
        case None => Typing.fail(TypeFailure.UnknownAssignablePort(name)).now
    )

  def checkComponent(component: Component): Unit < Typing.Global = direct:
    val inputs = component.inputs.foldLeft(Map.empty[Identifier, Type]):
      case (ports, (name, tpe)) =>
        if ports.contains(name) then
          Typing.fail(TypeFailure.PortAlreadyDeclared(name)).now
          ports
        else ports.updated(name, tpe)

    val outputs = component.outputs.foldLeft(Map.empty[Identifier, Type]):
      case (ports, (name, tpe)) =>
        if ports.contains(name) || inputs.contains(name) then
          Typing.fail(TypeFailure.PortAlreadyDeclared(name)).now
          ports
        else ports.updated(name, tpe)

    val (subPorts, subcomponents) = component.subcomponents.foldLeft(
      (Set.empty[PortIdentifier], Map.empty[Identifier, ComponentIO])
    ):
      case ((ports, subs), (name, comp)) =>
        if subs.contains(name) || inputs.contains(name) || outputs.contains(name) then
          Typing.fail(TypeFailure.PortAlreadyDeclared(name)).now
          if GlobalContext.getComponent(comp).now.isEmpty then
            Typing.fail(TypeFailure.UnknownComponent(comp)).now
          (ports, subs)
        else
          GlobalContext.getComponent(comp).now match
            case Some(io) =>
              val newIns = io.inputs.map((inName, _) => PortIdentifier.Sub(name, inName))
              (ports ++ newIns, subs.updated(name, io))
            case None =>
              Typing.fail(TypeFailure.UnknownComponent(comp)).now
              (ports, subs)

    val io = ComponentIO(inputs, outputs)
    val remainingPorts = outputs.map((name, _) => PortIdentifier.Main(name)).toSet ++ subPorts
    Var.run(ComponentContext(io, subcomponents, remainingPorts))(checkBody(component.body)).now
    GlobalContext.declareComponent(component.name, io).now

  def checkProgram(program: Program): Unit < Typing.Global =
    Kyo.foreachDiscard(program.components)(checkComponent)