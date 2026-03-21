package io.github.iltotore.redhdl.ast

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import kyo.Chunk

/**
 * AST node representing a Boolean expression in the RedHDL source language.
 *
 * Expressions appear on the right-hand side of port assignments inside a
 * component body and are recursively composed from literals, port reads,
 * and logical operators.
 */
enum Expr:
  /** A Boolean literal (`true` or `false`). */
  case LBool(value: Boolean)

  /**
   * A read of the signal currently present on an input port.
   *
   * @param identifier The port to read; may be a top-level input (`Main`) or
   *                   an output port of an instantiated sub-component (`Sub`).
   */
  case InputCall(identifier: PortIdentifier)

  /**
   * Logical NOT of a single sub-expression.
   *
   * @param expr The expression whose value is inverted.
   */
  case Not(expr: Expr)

  /**
   * Logical OR of two sub-expressions.
   *
   * @param left  The first operand.
   * @param right The second operand.
   */
  case Or(left: Expr, right: Expr)

  /**
   * Logical AND of two sub-expressions.
   *
   * @param left  The first operand.
   * @param right The second operand.
   */
  case And(left: Expr, right: Expr)

  /**
   * Logical XOR of two sub-expressions.
   *
   * @param left  The first operand.
   * @param right The second operand.
   */
  case Xor(left: Expr, right: Expr)
