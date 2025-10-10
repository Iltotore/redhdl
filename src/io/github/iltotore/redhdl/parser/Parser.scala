package io.github.iltotore.redhdl.parser

import kyo.*
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type
import io.github.iltotore.redhdl.ast.untpd.Component
import io.github.iltotore.redhdl.ast.untpd.Expr
import io.github.iltotore.redhdl.ast.untpd.Program

object Parser:

  val parseLiteral: Expr < Parse[Token] = Parse.anyMatch:
    case Token.LBool(value) => Expr.LBool(value)
    case Token.MainIdent(name) => Expr.InputCall(PortIdentifier.Main(name))
    case Token.SubIdent(sub, name) => Expr.InputCall(PortIdentifier.Sub(sub, name))

  val parseIdentifier: Identifier < Parse[Token] = Parse.anyMatch:
    case Token.MainIdent(identifier) => identifier

  lazy val parseExpr: Expr < Parse[Token] = Parse.firstOf(
    parseLiteral,
    Parse.between(
      Parse.literal(Token.ParenOpen),
      parseExpr,
      Parse.literal(Token.ParenClosed)
    )
  )

  val parseType: Type < Parse[Token] = Parse
    .literal(Token.MainIdent(Identifier("Boolean")))
    .andThen(Type.Bool)

  val parseParam: (Identifier, Type) < Parse[Token] = Parse.inOrder(
    parseIdentifier,
    Parse.literal(Token.Colon),
    parseType
  ).map((id, _, tpe) => (id, tpe))

  val parseBody: Chunk[(PortIdentifier, Expr)] < Parse[Token] = Parse.between(
    Parse.literal(Token.Begin),
    Parse.repeat(
      Parse.inOrder(
        Parse.anyMatch[PortIdentifier][Token]:
          case Token.MainIdent(name) => PortIdentifier.Main(name)
          case Token.SubIdent(sub, name) => PortIdentifier.Sub(sub, name),
        Parse.literal(Token.Equal),
        parseExpr
      ).map((id, _, expr) => (id, expr))
    ),
    Parse.literal(Token.End)
  )

  val parseInputs: Chunk[(Identifier, Type)] < Parse[Token] = Parse.inOrder(
    Parse.literal(Token.Input),
    Parse.separatedBy(parseParam, Parse.literal(Token.Comma))
  ).map(_._2)

  val parseOutputs: Chunk[(Identifier, Type)] < Parse[Token] = Parse.inOrder(
    Parse.literal(Token.Output),
    Parse.separatedBy(parseParam, Parse.literal(Token.Comma))
  ).map(_._2)

  val parseComponent: Component < Parse[Token] = Parse.inOrder(
    Parse.literal(Token.Component),
    parseIdentifier,
    Parse.attempt(parseInputs),
    Parse.attempt(parseOutputs),
    parseBody
  ).map((_, name, inputs, outputs, body) => Component(name, inputs.getOrElse(Chunk.empty), outputs.getOrElse(Chunk.empty), body))

  val parseProgram: Program < Parse[Token] = Parse.entireInput(
    Parse.repeat(parseComponent).map(Program.apply)
  )
