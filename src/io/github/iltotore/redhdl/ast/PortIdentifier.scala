package io.github.iltotore.redhdl.ast

enum PortIdentifier:
  case Main(name: Identifier)
  case Sub(subComponent: Identifier, name: Identifier)

  def asMain: Identifier = this match
    case PortIdentifier.Main(name)              => name
    case PortIdentifier.Sub(subComponent, name) => throw AssertionError(s"Subcomponent port in expanded component: $subComponent.$name")
