package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type
import kyo.*

/**
 * The global type-checking state, holding all component declarations seen so far.
 *
 * [[GlobalContext]] is threaded through the program-level type-checking pass via
 * the [[Typing.Global]] effect (a [[kyo.Var]]).  As each component is successfully
 * type-checked it is added to the context, making it available as a sub-component
 * type for subsequent declarations.
 *
 * @param components The map from component [[Identifier]] to its [[ComponentInfo]],
 *                   populated incrementally during the type-checking pass.
 */
case class GlobalContext(
    components: Map[Identifier, ComponentInfo]
):

  /**
   * Look up a component by name.
   *
   * @param name The component identifier to retrieve.
   * @return `Some(info)` if the component is registered, `None` otherwise.
   */
  def getComponent(name: Identifier): Option[ComponentInfo] =
    components.get(name)

  /**
   * Register a new component declaration, returning the updated context.
   *
   * @param name The component's name.
   * @param io   The [[ComponentInfo]] to store.
   * @return A new [[GlobalContext]] with `name → io` added to [[components]].
   */
  def declareComponent(name: Identifier, io: ComponentInfo): GlobalContext =
    this.copy(components = components.updated(name, io))

/**
 * Pre-populated default context and [[Typing.Global]] accessor helpers.
 */
object GlobalContext:

  /**
   * The initial [[GlobalContext]] pre-populated with the three built-in primitive
   * components: `Not`, `Or`, and `And`.
   *
   * These primitives are always available without explicit declaration and map
   * directly to their corresponding Minecraft gate schematics.
   */
  val default: GlobalContext = GlobalContext(Map(
    Identifier("Not") -> ComponentInfo(
      io = ComponentIO(
        inputs = Map(Identifier("in") -> Type.Bool),
        outputs = Map(Identifier("out") -> Type.Bool)
      ),
      subcomponents = Chunk.empty,
      body = Chunk(
        PortIdentifier.Main(Identifier("out")) -> Expr.Not(
          Expr.InputCall(PortIdentifier.Main(Identifier("in")))
        )
      )
    ),
    Identifier("Or") -> ComponentInfo(
      io = ComponentIO(
        inputs = Map(
          Identifier("inA") -> Type.Bool,
          Identifier("inB") -> Type.Bool
        ),
        outputs = Map(Identifier("out") -> Type.Bool)
      ),
      subcomponents = Chunk.empty,
      body = Chunk(
        PortIdentifier.Main(Identifier("out")) -> Expr.Or(
          Expr.InputCall(PortIdentifier.Main(Identifier("inA"))),
          Expr.InputCall(PortIdentifier.Main(Identifier("inB")))
        )
      )
    ),
    Identifier("And") -> ComponentInfo(
      io = ComponentIO(
        inputs = Map(
          Identifier("inA") -> Type.Bool,
          Identifier("inB") -> Type.Bool
        ),
        outputs = Map(Identifier("out") -> Type.Bool)
      ),
      subcomponents = Chunk.empty,
      body = Chunk(
        PortIdentifier.Main(Identifier("out")) -> Expr.And(
          Expr.InputCall(PortIdentifier.Main(Identifier("inA"))),
          Expr.InputCall(PortIdentifier.Main(Identifier("inB")))
        )
      )
    )
  ))

  /**
   * Look up a component from the ambient [[GlobalContext]] state.
   *
   * @param name The component identifier to retrieve.
   * @return `Some(info)` if the component is registered, `None` otherwise, within
   *         [[Typing.Global]].
   */
  def getComponent(name: Identifier): Option[ComponentInfo] < Typing.Global =
    Var.use[GlobalContext](_.getComponent(name))

  /**
   * Register a component declaration in the ambient [[GlobalContext]] state.
   *
   * @param name The component's name.
   * @param io   The [[ComponentInfo]] to store.
   */
  def declareComponent(name: Identifier, io: ComponentInfo): Unit < Typing.Global =
    Var.use[GlobalContext](_.declareComponent(name, io))
      .map(Var.setDiscard)
