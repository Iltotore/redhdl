package io.github.iltotore.redhdl.minecraft

enum SchematicFailure:
  case MissingSchematic(tpe: GateType)
  case InvalidSchematic(path: String, message: String)
