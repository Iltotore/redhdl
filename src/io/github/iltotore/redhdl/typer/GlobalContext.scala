package io.github.iltotore.redhdl.typer

import kyo.*
import io.github.iltotore.redhdl.ast.Expr
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.PortIdentifier
import io.github.iltotore.redhdl.ast.Type

case class GlobalContext(
  components: Map[Identifier, ComponentInfo]
):

  def getComponent(name: Identifier): Option[ComponentInfo] =
    components.get(name)

  def declareComponent(name: Identifier, io: ComponentInfo): GlobalContext =
    this.copy(components = components.updated(name, io))

object GlobalContext:
  val default: GlobalContext = GlobalContext(Map(
    Identifier("Not") -> ComponentInfo(
      io = ComponentIO(
        inputs = Map(Identifier("in") -> Type.Bool),
        outputs = Map(Identifier("out") -> Type.Bool)
      ),
      subcomponents = Chunk.empty,
      body = Chunk(
        PortIdentifier.Main(Identifier("out")) -> Expr.Not(
          Expr.InputCall(PortIdentifier.Main(Identifier("out")))
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
  
  def getComponent(name: Identifier): Option[ComponentInfo] < Typing.Global =
    Var.use[GlobalContext](_.getComponent(name))

  def declareComponent(name: Identifier, io: ComponentInfo): Unit < Typing.Global =
    Var.use[GlobalContext](_.declareComponent(name, io))
      .map(Var.setDiscard)