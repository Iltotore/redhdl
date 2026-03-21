package io.github.iltotore.redhdl.parser

import io.github.iltotore.redhdl.ast.Component
import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Program
import io.github.iltotore.redhdl.ast.Type
import kyo.*

/**
 * The syntactic analysis phase of the RedHDL compiler.
 *
 * [[Parser]] converts the flat [[Token]] stream produced by [[Lexer]] into the
 * structured [[Program]] AST.  It is built on top of the Kyo `Parse` combinator
 * library and exposes a set of named parsers that are composed bottom-up.
 *
 * The grammar recognised is roughly:
 * {{{
 *   program    ::= component*
 *   component  ::= "component" ident inputs? outputs? subcomponents? body
 *   inputs     ::= "input" param ("," param)*
 *   outputs    ::= "output" param ("," param)*
 *   subcomponents ::= "subcomponent" subcomponent ("," subcomponent)*
 *   body       ::= "begin" assignment* "end"
 *   assignment ::= portIdent "=" expr
 *   expr       ::= prefix (op prefix)*
 *   prefix     ::= "not" term | term
 *   term       ::= literal | ident | "(" expr ")"
 * }}}
 */
object Parser:

  /**
   * Parse a sequence of `element`s separated by a binary `sep` parser that returns
   * a combining function, folding them left-to-right.
   *
   * @tparam A  The element type.
   * @tparam In The input token type.
   * @param element The parser for a single element.
   * @param sep     A parser that consumes a separator and returns the combining function.
   * @return A parser that produces the left-folded result.
   */
  private def separatedByReduce[A, In](element: A < Parse[In], sep: ((A, A) => A) < Parse[In])(using Tag[In], Frame): A < Parse[In] =
    Parse
      .inOrder(
        element,
        Parse.repeat(Parse.inOrder(sep, Parse.require(element)))
      )
      .map((firstTerm, others) =>
        others.foldLeft(firstTerm):
          case (left, (reduce, right)) => reduce(left, right)
      )

  /**
   * Parse a left-associative binary expression whose operands are `element`s and
   * whose operators are drawn from `operators`.
   *
   * @tparam A  The element/result type.
   * @tparam In The input token type.
   * @param element   The parser for a single operand.
   * @param operators A map from operator token to the corresponding combining function.
   * @return A parser that produces the folded expression.
   */
  private def binaryOperator[A, In](element: A < Parse[In], operators: Map[In, (A, A) => A])(using Tag[In], Frame): A < Parse[In] =
    separatedByReduce(
      element,
      Parse.anyMatch(operators.get.unlift)
    )

  /**
   * Parse a literal value or a port input-call expression.
   *
   * Matches:
   *   - [[Token.LBool]] → [[Expr.LBool]]
   *   - [[Token.MainIdent]] → [[Expr.InputCall]] with a [[PortIdentifier.Main]]
   *   - [[Token.SubIdent]] → [[Expr.InputCall]] with a [[PortIdentifier.Sub]]
   */
  val parseLiteral: Expr < Parse[Token] = Parse.anyMatch:
    case Token.LBool(value)        => Expr.LBool(value)
    case Token.MainIdent(name)     => Expr.InputCall(PortIdentifier.Main(name))
    case Token.SubIdent(sub, name) => Expr.InputCall(PortIdentifier.Sub(sub, name))

  /**
   * Parse a plain [[Identifier]] from a [[Token.MainIdent]] token.
   */
  val parseIdentifier: Identifier < Parse[Token] = Parse.anyMatch:
    case Token.MainIdent(identifier) => identifier

  /**
   * Parse a single expression term: either a literal/call or a parenthesised
   * sub-expression.
   *
   * The `lazy val` is required because `parseExpr` and `parseTerm` are mutually
   * recursive.
   */
  lazy val parseTerm: Expr < Parse[Token] = Parse.firstOf(
    parseLiteral,
    Parse.between(
      Parse.literal(Token.ParenOpen),
      parseExpr,
      Parse.literal(Token.ParenClosed)
    )
  )

  /**
   * Map of prefix-operator tokens to their corresponding [[Expr]] constructors.
   */
  private val prefixOps: Map[Token, Expr => Expr] = Map(
    Token.Not -> Expr.Not.apply
  )

  /**
   * Map of binary Boolean operator tokens to their corresponding [[Expr]] constructors.
   */
  private val boolOps: Map[Token, (Expr, Expr) => Expr] = Map(
    Token.Or -> Expr.Or.apply,
    Token.And -> Expr.And.apply,
    Token.Xor -> Expr.Xor.apply
  )

  /**
   * Parse a prefix expression: an optional `not` followed by a term.
   *
   * The `lazy val` is required due to the mutual recursion via [[parseTerm]].
   */
  lazy val parsePrefix: Expr < Parse[Token] = Parse.firstOf(
    Parse.inOrder(
      Parse.anyMatch(prefixOps.get.unlift),
      parseTerm
    ).map(_(_)),
    parseTerm
  )

  /**
   * Parse a comparison-level expression (currently Boolean binary operators:
   * `or`, `and`, `xor`).
   */
  lazy val parseComparison: Expr < Parse[Token] = binaryOperator(parsePrefix, boolOps)

  /**
   * Parse a full expression at the highest precedence level.
   *
   * Currently an alias for [[parseComparison]].
   */
  lazy val parseExpr: Expr < Parse[Token] = parseComparison

  /**
   * Parse a type annotation.
   *
   * Currently only `Boolean` is accepted, producing [[Type.Bool]].
   */
  val parseType: Type < Parse[Token] = Parse
    .literal(Token.MainIdent(Identifier("Boolean")))
    .andThen(Type.Bool)

  /**
   * Parse a `name: Type` parameter declaration.
   *
   * @return A pair of the parameter name and its [[Type]].
   */
  val parseParam: (Identifier, Type) < Parse[Token] = Parse.inOrder(
    parseIdentifier,
    Parse.literal(Token.Colon),
    parseType
  ).map((id, _, tpe) => (id, tpe))

  /**
   * Parse a `name: ComponentType` sub-component declaration.
   *
   * @return A pair of the local instance name and the referenced component type name.
   */
  val parseSubcomponent: (Identifier, Identifier) < Parse[Token] = Parse.inOrder(
    parseIdentifier,
    Parse.literal(Token.Colon),
    parseIdentifier
  ).map((id, _, sub) => (id, sub))

  /**
   * Parse the body block of a component, delimited by `begin` … `end`.
   *
   * Each line inside the body is a port assignment: `portIdent = expr`.
   *
   * @return A chunk of `(PortIdentifier, Expr)` assignment pairs.
   */
  val parseBody: Chunk[(PortIdentifier, Expr)] < Parse[Token] = Parse.between(
    Parse.literal(Token.Begin),
    Parse.repeat(
      Parse.inOrder(
        Parse.anyMatch[PortIdentifier][Token]:
          case Token.MainIdent(name)     => PortIdentifier.Main(name)
          case Token.SubIdent(sub, name) => PortIdentifier.Sub(sub, name),
        Parse.literal(Token.Equal),
        parseExpr
      ).map((id, _, expr) => (id, expr))
    ),
    Parse.literal(Token.End)
  )

  /**
   * Parse an `input` clause: `"input" param ("," param)*`.
   *
   * @return A chunk of input port `(Identifier, Type)` pairs.
   */
  val parseInputs: Chunk[(Identifier, Type)] < Parse[Token] = Parse.inOrder(
    Parse.literal(Token.Input),
    Parse.separatedBy(parseParam, Parse.literal(Token.Comma))
  ).map(_._2)

  /**
   * Parse an `output` clause: `"output" param ("," param)*`.
   *
   * @return A chunk of output port `(Identifier, Type)` pairs.
   */
  val parseOutputs: Chunk[(Identifier, Type)] < Parse[Token] = Parse.inOrder(
    Parse.literal(Token.Output),
    Parse.separatedBy(parseParam, Parse.literal(Token.Comma))
  ).map(_._2)

  /**
   * Parse a `subcomponent` clause: `"subcomponent" subcomponent ("," subcomponent)*`.
   *
   * @return A chunk of `(instanceName, componentType)` pairs.
   */
  val parseSubcomponents: Chunk[(Identifier, Identifier)] < Parse[Token] = Parse.inOrder(
    Parse.literal(Token.Subcomponent),
    Parse.separatedBy(parseSubcomponent, Parse.literal(Token.Comma))
  ).map(_._2)

  /**
   * Parse a complete component declaration.
   *
   * The `input`, `output`, and `subcomponent` clauses are all optional
   * (wrapped in [[Parse.attempt]]).
   *
   * @return A [[Component]] AST node.
   */
  val parseComponent: Component < Parse[Token] = Parse.inOrder(
    Parse.literal(Token.Component),
    parseIdentifier,
    Parse.attempt(parseInputs),
    Parse.attempt(parseOutputs),
    Parse.attempt(parseSubcomponents),
    parseBody
  ).map((_, name, inputs, outputs, subcomponents, body) =>
    Component(
      name,
      inputs.getOrElse(Chunk.empty),
      outputs.getOrElse(Chunk.empty),
      subcomponents.getOrElse(Chunk.empty),
      body
    )
  )

  /**
   * Parse an entire token stream as a [[Program]].
   *
   * Expects zero or more [[parseComponent]] declarations followed by the end of
   * input; fails if any tokens remain after the last component.
   *
   * @return The parsed [[Program]].
   */
  val parseProgram: Program < Parse[Token] = Parse.entireInput(
    Parse.repeat(parseComponent).map(Program.apply)
  )
