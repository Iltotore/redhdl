package io.github.iltotore.redhdl.parser

import io.github.iltotore.redhdl.ast.Identifier

enum Token derives CanEqual:
  case LBool(value: Boolean)
  case MainIdent(identifier: Identifier)
  case SubIdent(subComponent: Identifier, name: Identifier)

  case NewLine

  // Symbols
  case ParenOpen // (
  case ParenClosed // )
  case Comma // ,
  case Colon // :
  case Equal // =

  // Keywords
  case Component
  case Subcomponent
  case Input
  case Output
  case Begin
  case End
  case Not
  case Or
  case And
  case Xor
