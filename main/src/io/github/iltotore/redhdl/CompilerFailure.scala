package io.github.iltotore.redhdl

import io.github.iltotore.redhdl.minecraft.SchematicFailure
import io.github.iltotore.redhdl.typer.TypeFailure
import kyo.ParseFailure

/**
 * The union of all failure kinds that the RedHDL compiler can produce.
 *
 * The three constituent types correspond to the three major compilation phases:
 *   - [[kyo.ParseFailure]]                 – lexing or parsing errors.
 *   - [[io.github.iltotore.redhdl.typer.TypeFailure]]      – type-checking errors.
 *   - [[io.github.iltotore.redhdl.minecraft.SchematicFailure]] – schematic-generation errors.
 *
 * Failures are accumulated via the [[kyo.Emit]] channel of the [[Compilation]] effect
 * and collected by [[Compilation.run]].
 */
type CompilerFailure = ParseFailure | TypeFailure | SchematicFailure

extension (failure: CompilerFailure)
  /**
   * Format this failure as a human-readable string suitable for display in the CLI.
   *
   * Each variant produces a single-line message with enough context for the user
   * to locate and understand the problem.
   *
   * @return A non-empty, human-readable description of the failure.
   */
  def toPrettyString: String = failure match
    case ParseFailure(message, position)                  => s"Parse error at ${position}: $message"
    case SchematicFailure.MissingSchematic(tpe)           => s"Schematic of type $tpe not found"
    case SchematicFailure.InvalidSchematic(path, message) => s"Invalid schematic at $path: $message"
    case TypeFailure.Mismatch(got, expected)              => s"Type mismatch: expected $expected, got $got"
    case TypeFailure.UnknownCallablePort(name)            => s"Unknown callable port: $name"
    case TypeFailure.UnknownAssignablePort(name)          => s"Unknown assignable port: $name"
    case TypeFailure.UnknownComponent(name)               => s"Unknown component: $name"
    case TypeFailure.UnusedPort(identifier)               => s"Port $identifier is declared but never used"
    case TypeFailure.PortAlreadyDeclared(identifier)      => s"Port $identifier is already declared"
    case TypeFailure.UnknownEntrypoint(identifier)        => s"Unknown entrypoint: $identifier"
