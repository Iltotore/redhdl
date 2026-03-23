package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Component
import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Program
import io.github.iltotore.redhdl.ast.Type
import kyo.*

/**
 * The type-checking pass of the RedHDL compiler.
 *
 * [[TypeChecker]] validates a parsed [[Program]] against the RedHDL type rules
 * and produces a [[GlobalContext]] that maps every component name to its
 * [[ComponentInfo]].  It runs within the [[Typing.Global]] effect, emitting
 * [[TypeFailure]] diagnostics for every violation found.
 *
 * The checking strategy is:
 *   1. For each component in program order, collect its declared ports, validate
 *      sub-component references, and check the body assignments.
 *   2. Register the component in the [[GlobalContext]] so that subsequent components
 *      can reference it as a sub-component type.
 */
object TypeChecker:

  /**
   * Assert that `tpe` equals `expected`, emitting a [[TypeFailure.Mismatch]]
   * diagnostic if they differ.
   *
   * @param tpe      The actual type found in the expression.
   * @param expected The type required by the port or context.
   */
  def assertType(tpe: Type, expected: Type): Unit < Typing = direct:
    if tpe == expected then ()
    else Typing.fail(TypeFailure.Mismatch(tpe, expected)).now

  /**
   * Check that the type inferred for `expr` matches `expected`.
   *
   * @param expr     The expression whose type is to be checked.
   * @param expected The expected [[Type]].
   */
  def assertExprType(expr: Expr, expected: Type): Unit < Typing.Component =
    getType(expr).map(assertType(_, expected))

  /**
   * Infer the [[Type]] of a Boolean expression within a component context.
   *
   * For port references, the callable type is resolved from the ambient
   * [[ComponentContext]].  Compound expressions are checked recursively and always
   * produce [[Type.Bool]].
   *
   * @param expr The expression to type-check.
   * @return The inferred [[Type]] within [[Typing.Component]].
   */
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
      case Expr.Xor(left, right) =>
        assertExprType(left, Type.Bool).now
        assertExprType(right, Type.Bool).now
        Type.Bool

  /**
   * Check every assignment in a component body.
   *
   * For each `(port, expr)` pair the expression type is inferred and compared against
   * the declared type of the assigned port.  An [[TypeFailure.UnknownAssignablePort]]
   * is emitted if the port is not in scope.
   *
   * @param body The sequence of port assignments to validate.
   */
  def checkBody(body: Chunk[(PortIdentifier, Expr)]): Unit < Typing.Component = direct:
    body.foreach((name, expr) =>
      val exprType = getType(expr).now
      ComponentContext.getAssignableType(name).now match
        case Some(tpe) => assertType(exprType, tpe).now
        case None      => Typing.fail(TypeFailure.UnknownAssignablePort(name)).now
    )

  /**
   * Validate a single [[Component]] declaration and register it in the global context.
   *
   * Steps:
   *   1. Collect and de-duplicate input ports; emit [[TypeFailure.PortAlreadyDeclared]]
   *      for duplicates.
   *   2. Collect and de-duplicate output ports; same error for duplicates.
   *   3. Resolve each sub-component type; emit [[TypeFailure.UnknownComponent]] for
   *      unknown types and [[TypeFailure.PortAlreadyDeclared]] for duplicate instance
   *      names.
   *   4. Run [[checkBody]] to type-check all assignments.
   *   5. Register the component with [[GlobalContext.declareComponent]].
   *
   * @param component The [[Component]] AST node to check.
   */
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
            case Some(info) =>
              val newIns = info.io.inputs.map((inName, _) => PortIdentifier.Sub(name, inName))
              (ports ++ newIns, subs.updated(name, info.io))
            case None =>
              Typing.fail(TypeFailure.UnknownComponent(comp)).now
              (ports, subs)

    val info = ComponentInfo(ComponentIO(inputs, outputs), component.subcomponents, component.body)
    val remainingPorts = outputs.map((name, _) => PortIdentifier.Main(name)).toSet ++ subPorts
    Var.run(ComponentContext(info.io, subcomponents, remainingPorts))(checkBody(component.body)).now
    GlobalContext.declareComponent(component.name, info).now

  /**
   * Validate every component in a [[Program]] in declaration order.
   *
   * @param program The program to type-check.
   */
  def checkProgram(program: Program): Unit < Typing.Global =
    Kyo.foreachDiscard(program.components)(checkComponent)
