package io.github.iltotore.redhdl.ast.untpd

import io.github.iltotore.redhdl.ast.Identifier

/*
component And
input
  inA: Boolean,
  inB: Boolean
output
  out: Boolean
begin
  out = Not(
    Or(
      Not(inA),
      Not(inB)
    )
  )
end

 */

enum Expr:
  case LBool(value: Boolean)

  case InputCall(name: Identifier)
  case OuputAssign(name: Identifier, expr: Expr)
  case ComponentCall(name: Identifier, args: List[Expr])
