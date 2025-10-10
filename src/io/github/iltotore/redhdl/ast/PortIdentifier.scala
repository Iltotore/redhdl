package io.github.iltotore.redhdl.ast

enum PortIdentifier:
  case Main(name: Identifier)
  case Sub(subComponent: Identifier, name: Identifier)