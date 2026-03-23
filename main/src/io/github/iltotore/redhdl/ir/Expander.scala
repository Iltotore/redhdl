package io.github.iltotore.redhdl.ir

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type
import io.github.iltotore.redhdl.ir.ExpandedComponent
import io.github.iltotore.redhdl.ir.Expansion
import io.github.iltotore.redhdl.typer.ComponentInfo
import kyo.*

/**
 * Expands a [[io.github.iltotore.redhdl.typer.ComponentInfo]] into an
 * [[ExpandedComponent]] by recursively inlining all sub-component instantiations.
 *
 * Sub-component references are resolved from the [[Expansion]] environment (a map
 * from component name to [[ComponentInfo]]).  Each sub-component's ports and body
 * are renamed by prepending the instance name and the separator `$`, ensuring
 * name uniqueness after flattening.
 */
object Expander:

  /**
   * Combine a prefix and a suffix into a single prefixed [[Identifier]].
   *
   * The resulting identifier is `"<prefix>$<suffix>"`.  The `$` character is used
   * as a separator because it is not a valid character in user-written identifiers,
   * guaranteeing that generated names never clash with source-level names.
   *
   * @param prefix The instance name of the sub-component.
   * @param suffix The port name within that sub-component.
   * @return A fresh [[Identifier]] of the form `prefix$suffix`.
   */
  private def prefixIdentifier(prefix: Identifier, suffix: Identifier): Identifier =
    Identifier.assume(s"$prefix$$$suffix")

  /**
   * Rewrite an [[Expr]] so that every [[PortIdentifier.Sub]] reference is replaced
   * by an equivalent [[PortIdentifier.Main]] reference using the inlined name.
   *
   * [[PortIdentifier.Main]] references are left unchanged.
   *
   * @param expr The expression to rewrite.
   * @return The rewritten expression with all sub-component port references
   *         converted to flat main-port references.
   */
  def expandExpr(expr: Expr): Expr = expr match
    case Expr.LBool(value) => Expr.LBool(value)
    case Expr.InputCall(identifier) =>
      identifier match
        case PortIdentifier.Main(name) =>
          Expr.InputCall(PortIdentifier.Main(name))
        case PortIdentifier.Sub(subComponent, name) =>
          Expr.InputCall(PortIdentifier.Main(prefixIdentifier(subComponent, name)))

    case Expr.Not(expr)        => Expr.Not(expandExpr(expr))
    case Expr.Or(left, right)  => Expr.Or(expandExpr(left), expandExpr(right))
    case Expr.And(left, right) => Expr.And(expandExpr(left), expandExpr(right))
    case Expr.Xor(left, right) => Expr.Xor(expandExpr(left), expandExpr(right))

  /**
   * Rewrite an [[Expr]] by prepending `prefix$` to every [[PortIdentifier.Main]]
   * reference inside it.
   *
   * This is used when inlining a sub-component's body into the parent: the
   * sub-component's ports are already flat (`Main`) after [[expandExpr]] but still
   * carry their original names; `prefixExpr` renames them to avoid collisions.
   *
   * @param prefix The instance name to prepend.
   * @param expr   The expression whose port references are to be prefixed.
   * @return The renamed expression.
   * @throws AssertionError if a [[PortIdentifier.Sub]] reference is encountered,
   *                        which should never occur after [[expandExpr]].
   */
  def prefixExpr(prefix: Identifier, expr: Expr): Expr = expr match
    case Expr.LBool(value) => Expr.LBool(value)
    case Expr.InputCall(identifier) =>
      identifier match
        case PortIdentifier.Main(name)              => Expr.InputCall(PortIdentifier.Main(prefixIdentifier(prefix, name)))
        case PortIdentifier.Sub(subComponent, name) => throw AssertionError(s"Subcomponent port in expanded component: $subComponent.$name")
    case Expr.Not(expr)        => Expr.Not(prefixExpr(prefix, expr))
    case Expr.Or(left, right)  => Expr.Or(prefixExpr(prefix, left), prefixExpr(prefix, right))
    case Expr.And(left, right) => Expr.And(prefixExpr(prefix, left), prefixExpr(prefix, right))
    case Expr.Xor(left, right) => Expr.Xor(prefixExpr(prefix, left), prefixExpr(prefix, right))

  /**
   * Recursively expand a [[ComponentInfo]] into an [[ExpandedComponent]].
   *
   * The algorithm:
   *   1. Flatten the component's own body, replacing `Sub` port identifiers with
   *      prefixed `Main` identifiers via [[expandExpr]].
   *   2. For each sub-component instantiation, recursively expand the referenced
   *      component, rename all its ports with [[prefixIdentifier]] / [[prefixExpr]],
   *      and merge the result into the running accumulator.
   *
   * The [[Expansion]] environment provides the [[ComponentInfo]] for each referenced
   * component name.
   *
   * @param component The component to expand.
   * @return An [[ExpandedComponent]] within the [[Expansion]] effect, which
   *         provides the lookup environment for sub-component types.
   */
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
