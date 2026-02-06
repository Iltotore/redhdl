package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Program
import kyo.*

type Typing = Emit[TypeFailure] & Abort[Unit]

object Typing:

  type Global = Var[GlobalContext] & Typing
  type Component = Var[ComponentContext] & Typing

  def runGlobal[S](body: Unit < (Typing.Global & S)): Result[Chunk[TypeFailure], Map[Identifier, ComponentInfo]] < S =
    body.handle(
      Var.runTuple(GlobalContext.default),
      Abort.runPartialOrThrow,
      Emit.run(_)
    )
      .map((failures, out) =>
        out match
          case Result.Success((ctx, _)) if failures.isEmpty => Result.Success(ctx.components)
          case _                                            => Result.Failure(failures)
      )

  def fail(failure: TypeFailure): Unit < Typing = Emit.value(failure)

  def failAndAbort(failure: TypeFailure): Nothing < Typing =
    fail(failure).andThen(Abort.fail(()))

  def abortIfFail[A](body: A < Typing): A < Typing =
    abortIfEmit.run(body)

  private def abortIfEmit: Isolate[Emit[TypeFailure], Any, Emit[TypeFailure] & Abort[Unit]] =
    new Isolate[Emit[TypeFailure], Any, Emit[TypeFailure] & Abort[Unit]]:

      type State = Chunk[TypeFailure]

      type Transform[A] = (Chunk[TypeFailure], A)

      def capture[A, S](f: State => A < S)(using Frame) =
        f(Chunk.empty)

      def isolate[A, S](state: Chunk[TypeFailure], v: A < (S & Emit[TypeFailure]))(using Frame) =
        Emit.run(v)

      def restore[A, S](v: (Chunk[TypeFailure], A) < S)(using Frame): A < (Emit[TypeFailure] & Abort[Unit] & S) =
        for
          (state, result) <- v
          result <- Loop(state: Seq[TypeFailure]):
            case Seq() => Loop.done(result)
            case head +: tail =>
              Emit.valueWith(head)(Loop.continue(tail))
        yield
          if state.isEmpty then result
          else Abort.fail(())
      end restore
