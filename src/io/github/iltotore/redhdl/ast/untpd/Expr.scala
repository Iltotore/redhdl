package io.github.iltotore.redhdl.ast.untpd

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import kyo.Chunk

/*
component And
input
  inA: Boolean,
  inB: Boolean
output
  out: Boolean
subcomponent
  notA: Not
  notB: Not
  notRes: Not
  or: Or
begin
  notA.in = inA
  notB.in = inB
  or.inA = notA.out
  or.inB = notB.out
  notRes.in = or.out
  out = notRes.out
end

not (not inA or not inB)
 */

enum Expr:
  case LBool(value: Boolean)
  case InputCall(identifier: PortIdentifier)
  
  case Not(expr: Expr)
  case Or(expr: Expr)
  case And(expr: Expr)
