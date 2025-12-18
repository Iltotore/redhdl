package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.typer.ComponentInfo
import kyo.*
import io.github.iltotore.redhdl.ast.Type
import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.PortIdentifier

object Expander:

  private def prefixIdentifier(prefix: Identifier, suffix: Identifier): Identifier =
    Identifier.assume(s"$prefix$$$suffix")

  def expandComponent(component: ComponentInfo): ExpandedComponent < Expansion = direct:
    val flattenedBody = component.body.map:
      case (PortIdentifier.Main(name), expr) => (name, expr)
      case (PortIdentifier.Sub(sub, name), expr) => (prefixIdentifier(sub, name), expr)

    val (internalPorts, expandedBody) = component.subcomponents
      .foldLeft((Chunk.empty[(Identifier, Type)], flattenedBody)):
        case ((ports, body), (name, tpe)) =>
          val sub = expandComponent(Expansion.getComponent(tpe).now).now

          val renamedInputs = sub.io.inputs.map((subName, subType) => (prefixIdentifier(name, subName), subType))
          val renamedOutputs = sub.io.outputs.map((subName, subType) => (prefixIdentifier(name, subName), subType))

          val renamedBody = sub.body.map((subName, expr) => (prefixIdentifier(name, subName), expr))

          (ports ++ renamedInputs ++ renamedOutputs, body ++ renamedBody)

    ExpandedComponent(
      io = component.io,
      internalPorts = internalPorts,
      body = expandedBody
    )