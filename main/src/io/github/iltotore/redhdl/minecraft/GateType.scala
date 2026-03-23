package io.github.iltotore.redhdl.minecraft

import io.github.iltotore.redhdl.graph.AndSize
import io.github.iltotore.redhdl.graph.OrSize
import kyo.Chunk

/**
 * The set of logic gate types that have a corresponding pre-built schematic resource.
 *
 * Each variant maps to a `.schem` file stored under `resources/gates/` via
 * [[resourceName]].  The variants mirror the graph-level [[io.github.iltotore.redhdl.graph.NodeType]]
 * enum but are expressed in terms of the schematic layer rather than the graph layer,
 * and multi-input OR/AND gates carry their input count as a refined parameter.
 *
 * A [[CanEqual]] instance is derived so that [[GateType]] values can be compared with
 * `==` in maps and match expressions without implicit widening.
 */
enum GateType derives CanEqual:
  /** Named circuit input pin schematic. */
  case Input

  /** Named circuit output pin schematic. */
  case Output

  /** Constant-true source schematic. */
  case True

  /** Constant-false source schematic. */
  case False

  /** Logical NOT gate schematic. */
  case Not

  /**
   * Logical OR gate schematic with a specific input count.
   *
   * @param size The number of inputs (2–6), selecting the correct resource file.
   */
  case Or(size: OrSize)

  /**
   * Logical AND gate schematic with a specific input count.
   *
   * @param size The number of inputs (2–8), selecting the correct resource file.
   */
  case And(size: AndSize)

  /** Logical XOR gate schematic. */
  case Xor

  /** Signal relay / buffer schematic used for long-distance routing. */
  case Relay

  /**
   * The base file name (without extension or directory) of the schematic resource
   * for this gate type.
   *
   * The full resource path is `/gates/<resourceName>.schem`.
   *
   * @return The schematic resource name string.
   */
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

/**
 * Companion object for [[GateType]] containing the complete list of gate types
 * used by the schematic generator.
 */
object GateType:

  /**
   * All gate types for which a schematic resource must be loaded.
   *
   * The list includes every single-variant type plus every valid OR size (2–6) and
   * every valid AND size (2–8).
   */
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
