package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.graph.AndSize
import io.github.iltotore.redhdl.graph.OrSize
import kyo.Chunk

enum GateType derives CanEqual:
  case Input
  case Output
  case True
  case False
  case Not
  case Or(size: OrSize)
  case And(size: AndSize)
  case Xor
  case Relay

  def resourceName: String = this match
    case Input     => "input"
    case Output    => "output"
    case True      => "true"
    case False     => "false"
    case Not       => "not"
    case Or(size)  => s"or_$size"
    case And(size) => s"and_$size"
    case Xor       => "xor"
    case Relay     => "relay"

object GateType:

  val values = Chunk(
    GateType.Input,
    GateType.Output,
    GateType.True,
    GateType.False,
    GateType.Not,
    GateType.Xor,
    GateType.Relay
  )
    ++ Chunk.range(OrSize.MinValue, OrSize.MaxValue + 1).map(GateType.Or.apply)
    ++ Chunk.range(AndSize.MinValue, AndSize.MaxValue + 1).map(GateType.And.apply)