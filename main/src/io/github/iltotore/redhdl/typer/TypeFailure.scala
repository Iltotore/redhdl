package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type

/**
 * The set of type-checking failures that the RedHDL type checker can emit.
 *
 * Each variant corresponds to a distinct category of type error.  Failures are
 * emitted through the [[kyo.Emit]] channel of the [[Typing]] effect and collected
 * by [[io.github.iltotore.redhdl.Compilation.run]] as part of the unified
 * [[io.github.iltotore.redhdl.CompilerFailure]] union.
 */
enum TypeFailure:
  /**
   * The type of an expression did not match the type expected by the port it is
   * assigned to.
   *
   * @param got      The inferred type of the expression.
   * @param expected The type required by the port.
   */
  case Mismatch(got: Type, expected: Type)

  /**
   * An expression references a port that cannot be read (called) in this context.
   *
   * This occurs when an [[io.github.iltotore.redhdl.ast.Expr.InputCall]] references a
   * port name that is not in the component's input list or in any sub-component's
   * output list.
   *
   * @param name The [[PortIdentifier]] that could not be resolved.
   */
  case UnknownCallablePort(name: PortIdentifier)

  /**
   * A body assignment targets a port that cannot be written in this context.
   *
   * This occurs when the left-hand side of an assignment is not in the component's
   * output list and not in any sub-component's input list.
   *
   * @param name The [[PortIdentifier]] that could not be resolved.
   */
  case UnknownAssignablePort(name: PortIdentifier)

  /**
   * A sub-component declaration references a component type that has not been
   * declared.
   *
   * @param name The unknown component type [[Identifier]].
   */
  case UnknownComponent(name: Identifier)

  /**
   * A port was declared but never assigned a value in the component body.
   *
   * @param identifier The [[PortIdentifier]] of the unused port.
   */
  case UnusedPort(identifier: PortIdentifier)

  /**
   * A port name was declared more than once (either as two inputs, two outputs,
   * or as both an input and an output).
   *
   * @param identifier The duplicated port or instance name [[Identifier]].
   */
  case PortAlreadyDeclared(identifier: Identifier)

  /**
   * The compiler's entry-point component could not be found in the program.
   *
   * @param identifier The entry-point name that was requested but does not exist.
   */
  case UnknownEntrypoint(identifier: Identifier)
