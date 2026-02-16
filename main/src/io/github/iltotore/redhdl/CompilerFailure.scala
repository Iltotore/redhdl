package io.github.iltotore.redhdl

import io.github.iltotore.redhdl.minecraft.SchematicFailure
import io.github.iltotore.redhdl.typer.TypeFailure
import kyo.ParseFailure

type CompilerFailure = ParseFailure | TypeFailure | SchematicFailure

extension (failure: CompilerFailure)
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
