package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Identifier
import kyo.Chunk
import io.github.iltotore.redhdl.minecraft.GateType

enum NodeType derives CanEqual:
  case Input(name: Identifier)
  case Output(name: Identifier)

  case True
  case False
  case Not
  case Or
  case And
  case Xor
  case Relay

  def sizeX: Int = this match
    case Input(_)  => 1
    case Output(_) => 1
    case True      => 1
    case False     => 1
    case Not       => 1
    case Or        => 2
    case And       => 2
    case Xor       => 2
    case Relay     => 1

  def isInput: Boolean = this.isInstanceOf[Input]
  def isOutput: Boolean = this.isInstanceOf[Output]

  def toGateType: GateType = this match
    case Input(_) => GateType.Input
    case Output(_) => GateType.Output
    case True => GateType.True
    case False => GateType.False
    case Not => GateType.Not
    case Or => GateType.Or(OrSize(2))
    case And => GateType.And(AndSize(2))
    case Xor => GateType.Xor
    case Relay => GateType.Relay
  