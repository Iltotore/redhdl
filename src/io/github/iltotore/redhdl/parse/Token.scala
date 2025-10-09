package io.github.iltotore.redhdl.parse

import io.github.iltotore.redhdl.ast.Identifier

enum Token:
  case LBool(value: Boolean)
  case Ident(identifier: Identifier)

  case NewLine

  // Symbols
  case ParenOpen // (
  case ParenClosed // )
  case Comma // ,
  case Colon // :
  case Equal // =

  // Keywords
  case Component
  case Input
  case Output
  case Begin
  case End