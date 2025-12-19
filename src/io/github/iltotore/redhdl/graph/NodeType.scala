package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Identifier

enum NodeType:
  case Input(name: Identifier)
  case Output(name: Identifier)
  
  case True
  case False
  case Not
  case Or
  case And