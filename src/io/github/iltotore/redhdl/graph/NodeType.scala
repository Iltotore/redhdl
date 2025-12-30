package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Identifier

enum NodeType derives CanEqual:
  case Input(name: Identifier)
  case Output(name: Identifier)

  case True
  case False
  case Not
  case Or
  case And
  case Relay

  def width: Int = this match
    case Input(_)  => 1
    case Output(_) => 1
    case True      => 1
    case False     => 1
    case Not       => 1
    case Or        => 2
    case And       => 2
    case Relay     => 1
