package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.minecraft.GateType
import kyo.Chunk

/**
 * The kind of logic gate or I/O pin represented by a [[Node]] in the circuit graph.
 *
 * Each variant maps directly to a physical gate schematic in the Minecraft world
 * via [[toGateType]].  The `derives CanEqual` clause enables structural equality
 * comparisons, which is needed when pattern-matching on node types in routing and
 * schematic generation.
 */
enum NodeType derives CanEqual:
  /**
   * A named circuit input pin.
   *
   * @param name The port name as declared in the source component.
   */
  case Input(name: Identifier)

  /**
   * A named circuit output pin.
   *
   * @param name The port name as declared in the source component.
   */
  case Output(name: Identifier)

  /** A constant-true source node (logical 1). */
  case True

  /** A constant-false source node (logical 0). */
  case False

  /** A logical NOT gate (single-input inverter). */
  case Not

  /** A logical OR gate (two-input). */
  case Or

  /** A logical AND gate (two-input). */
  case And

  /** A logical XOR gate (two-input). */
  case Xor

  /**
   * A relay node inserted by [[io.github.iltotore.redhdl.graph.GraphRouter.addRelays]]
   * to bridge edges that span more than one layer.
   */
  case Relay

  /**
   * The number of horizontal pin columns this gate occupies in the schematic layout.
   *
   * Single-input and constant gates occupy 1 column; two-input gates occupy 2 columns
   * (one per operand).
   *
   * @return The width in pin columns.
   */
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

  /**
   * Whether this node type represents a circuit input pin.
   *
   * @return `true` if the node is an [[Input]].
   */
  def isInput: Boolean = this.isInstanceOf[Input]

  /**
   * Whether this node type represents a circuit output pin.
   *
   * @return `true` if the node is an [[Output]].
   */
  def isOutput: Boolean = this.isInstanceOf[Output]

  /**
   * Convert this graph node type to the corresponding [[GateType]] used for
   * schematic generation.
   *
   * Two-input OR and AND variants use the minimum gate size of 2.
   *
   * @return The [[GateType]] that should be pasted into the Minecraft schematic.
   */
  def toGateType: GateType = this match
    case Input(_)  => GateType.Input
    case Output(_) => GateType.Output
    case True      => GateType.True
    case False     => GateType.False
    case Not       => GateType.Not
    case Or        => GateType.Or(OrSize(2))
    case And       => GateType.And(AndSize(2))
    case Xor       => GateType.Xor
    case Relay     => GateType.Relay
