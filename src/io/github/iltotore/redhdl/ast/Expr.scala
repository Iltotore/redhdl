package io.github.iltotore.redhdl.ast

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import kyo.Chunk

enum Expr:
  case LBool(value: Boolean)
  case InputCall(identifier: PortIdentifier)
  
  case Not(expr: Expr)
  case Or(left: Expr, right: Expr)
  case And(left: Expr, right: Expr)
