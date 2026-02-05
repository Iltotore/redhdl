package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type

enum TypeFailure:
  case Mismatch(got: Type, expected: Type)
  case UnknownCallablePort(name: PortIdentifier)
  case UnknownAssignablePort(name: PortIdentifier)
  case UnknownComponent(name: Identifier)
  case UnusedPort(identifier: PortIdentifier)
  case PortAlreadyDeclared(identifier: Identifier)
