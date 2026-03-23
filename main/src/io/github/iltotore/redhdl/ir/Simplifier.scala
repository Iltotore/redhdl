package io.github.iltotore.redhdl.ir

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ir.ExpandedComponent
import io.github.iltotore.redhdl.ir.SimplifiedComponent
import kyo.Chunk

/**
 * Converts an [[ExpandedComponent]] into a [[SimplifiedComponent]] by inlining
 * internal port assignments and optionally applying algebraic simplifications.
 *
 * The simplifier traverses each output expression, replacing every reference to an
 * internal port with the expression assigned to that port.  When `optimize` is
 * `true`, the result of each recursive call is passed through [[optimizeSimplified]]
 * to reduce common constant and double-negation patterns.
 */
object Simplifier:

  /**
   * Produce a [[SimplifiedComponent]] from an [[ExpandedComponent]].
   *
   * For each output port declared in the component's I/O signature, the method
   * finds the corresponding body assignment, inlines all intermediate port
   * references via [[simplifyExpr]], and optionally optimises the result.
   *
   * @param optimize   Whether algebraic optimisations should be applied.
   * @param component  The expanded component to simplify.
   * @return A [[SimplifiedComponent]] with all internal wires eliminated.
   * @throws AssertionError if an output port has no corresponding body entry.
   */
  def simplifyComponent(optimize: Boolean)(component: ExpandedComponent): SimplifiedComponent =
    val simplifiedOutputs = component.io.outputs.map((out, _) =>
      val expr = component.body.collectFirst { case (i, expr) if i == out => expr }
        .getOrElse(throw AssertionError(s"Unassigned output: $out"))

      (out, simplifyExpr(component, expr, optimize))
    )

    SimplifiedComponent(Chunk.from(component.io.inputs.keys), simplifiedOutputs)

  /**
   * Apply one level of algebraic simplification to a Boolean expression.
   *
   * The following rules are applied (each applied at most once per call):
   *   - `not(not(x))` → `x`
   *   - `x and true`  → `x`
   *   - `true and x`  → `x`
   *   - `x and false` → `false`
   *   - `false and x` → `false`
   *   - `x or false`  → `x`
   *   - `false or x`  → `x`
   *   - `x or true`   → `true`
   *   - `true or x`   → `true`
   *   - `x xor false` → `x`
   *   - `false xor x` → `x`
   *
   * Any expression that does not match a rule is returned unchanged.
   *
   * @param expr The expression to simplify.
   * @return The simplified expression, or `expr` if no rule applies.
   */
  def optimizeSimplified(expr: Expr): Expr = expr match
    case Expr.Not(Expr.Not(expr))           => expr
    case Expr.And(left, Expr.LBool(true))   => left
    case Expr.And(Expr.LBool(true), right)  => right
    case Expr.And(_, Expr.LBool(false))     => Expr.LBool(false)
    case Expr.And(Expr.LBool(false), _)     => Expr.LBool(false)
    case Expr.Or(left, Expr.LBool(false))   => left
    case Expr.Or(Expr.LBool(false), right)  => right
    case Expr.Or(_, Expr.LBool(true))       => Expr.LBool(true)
    case Expr.Or(Expr.LBool(true), _)       => Expr.LBool(true)
    case Expr.Xor(left, Expr.LBool(false))  => left
    case Expr.Xor(Expr.LBool(false), right) => right
    case _                                  => expr

  /**
   * Recursively inline all internal port references in `expr` and optionally
   * optimise the result.
   *
   * A reference to a top-level input port is left as-is.  A reference to any other
   * port (internal or sub-component output) is replaced by the recursively simplified
   * expression assigned to that port in the component body.
   *
   * @param component The [[ExpandedComponent]] that provides the body lookup table.
   * @param expr      The expression to simplify.
   * @param optimize  Whether [[optimizeSimplified]] should be applied after each
   *                  recursive step.
   * @return The simplified (and optionally optimised) expression.
   * @throws AssertionError if a [[PortIdentifier.Sub]] reference is encountered,
   *                        which should not be present after expansion.
   */
  def simplifyExpr(component: ExpandedComponent, expr: Expr, optimize: Boolean): Expr =
    val simplified = expr match
      case Expr.LBool(value) => Expr.LBool(value)
      case Expr.InputCall(identifier) => identifier match
          case PortIdentifier.Sub(subComponent, name) => throw AssertionError(s"Subcomponent port in expanded component: $subComponent.$name")
          case PortIdentifier.Main(name) =>
            if component.io.inputs.contains(name) then Expr.InputCall(PortIdentifier.Main(name))
            else simplifyExpr(component, component.getExpr(name), optimize)

      case Expr.Not(expr)        => Expr.Not(simplifyExpr(component, expr, optimize))
      case Expr.Or(left, right)  => Expr.Or(simplifyExpr(component, left, optimize), simplifyExpr(component, right, optimize))
      case Expr.And(left, right) => Expr.And(simplifyExpr(component, left, optimize), simplifyExpr(component, right, optimize))
      case Expr.Xor(left, right) => Expr.Xor(simplifyExpr(component, left, optimize), simplifyExpr(component, right, optimize))

    if optimize then optimizeSimplified(simplified)
    else simplified
