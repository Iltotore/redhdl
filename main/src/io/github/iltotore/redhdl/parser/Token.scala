package io.github.iltotore.redhdl.parser

import io.github.iltotore.redhdl.ast.Identifier

/**
 * The set of tokens produced by the [[Lexer]] and consumed by the [[Parser]].
 *
 * Tokens represent the smallest meaningful units of the RedHDL source language.
 * The `derives CanEqual` clause enables structural equality comparisons, which is
 * needed when matching on token kinds inside parser combinators.
 */
enum Token derives CanEqual:
  /**
   * A Boolean literal (`true` or `false`).
   *
   * @param value The boolean value of the literal.
   */
  case LBool(value: Boolean)

  /**
   * A simple (unqualified) identifier, used for component names, port names, and
   * type names.
   *
   * @param identifier The non-blank [[Identifier]] string.
   */
  case MainIdent(identifier: Identifier)

  /**
   * A dotted, qualified identifier of the form `subComponent.port`, used to
   * reference a port on an instantiated sub-component.
   *
   * @param subComponent The local instance name of the sub-component.
   * @param name         The port name on the sub-component.
   */
  case SubIdent(subComponent: Identifier, name: Identifier)

  /** A newline character (currently not emitted by the lexer but reserved). */
  case NewLine

  // Symbols

  /** Opening parenthesis `(`. */
  case ParenOpen

  /** Closing parenthesis `)`. */
  case ParenClosed

  /** Comma `,`. */
  case Comma

  /** Colon `:`. */
  case Colon

  /** Equals sign `=`. */
  case Equal

  // Keywords

  /** The `component` keyword, introducing a component declaration. */
  case Component

  /** The `subcomponent` keyword, introducing a sub-component instantiation list. */
  case Subcomponent

  /** The `input` keyword, introducing the input port list. */
  case Input

  /** The `output` keyword, introducing the output port list. */
  case Output

  /** The `begin` keyword, opening the component body. */
  case Begin

  /** The `end` keyword, closing the component body. */
  case End

  /** The `not` keyword (logical NOT prefix operator). */
  case Not

  /** The `or` keyword (logical OR binary operator). */
  case Or

  /** The `and` keyword (logical AND binary operator). */
  case And

  /** The `xor` keyword (logical XOR binary operator). */
  case Xor
