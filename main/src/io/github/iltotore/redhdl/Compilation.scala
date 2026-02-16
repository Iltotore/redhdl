package io.github.iltotore.redhdl

import kyo.*

type Compilation = Emit[CompilerFailure] & Abort[Unit] & Env[CompilationContext] & Sync

object Compilation:

  def fromParseResult[A, S](result: ParseResult[A] < S): A < (Compilation & S) =
    result.map(r =>
      Kyo.foreach(r.errors)(Emit.value)
        .andThen(
          r.out match
            case Absent         => Abort.fail(())
            case Present(value) => value
        )
    )

  def run[A, S](context: CompilationContext)(body: A < (Compilation & S))(using Frame): Result[Chunk[CompilerFailure], A] < (S & Sync) =
    body.handle(
      Env.run(context),
      Abort.fold[Unit](Result.Success[A](_), _ => Result.Failure(Chunk.empty), Result.Panic.apply),
      Emit.run(_).map((failures, result) =>
        if failures.isEmpty then result
        else Result.Failure(failures)
      )
    )

  def emitAndAbort(failure: CompilerFailure): Nothing < Compilation =
    Emit.value(failure).andThen(Abort.fail(()))