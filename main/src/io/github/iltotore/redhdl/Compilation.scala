package io.github.iltotore.redhdl

import kyo.*

/**
 * The composite Kyo effect type that represents the full compilation pipeline.
 *
 * Any value of type `A < Compilation` may:
 *   - Emit zero or more [[CompilerFailure]] diagnostics via [[kyo.Emit]].
 *   - Abort the current computation early with [[kyo.Abort]][Unit].
 *   - Read compiler settings from the ambient [[CompilationContext]] via [[kyo.Env]].
 *   - Perform synchronous side effects via [[kyo.Sync]].
 */
type Compilation = Emit[CompilerFailure] & Abort[Unit] & Env[CompilationContext] & Sync

/**
 * Utilities for running and constructing [[Compilation]] effects.
 */
object Compilation:

  /**
   * Lift a [[ParseResult]] into the [[Compilation]] effect.
   *
   * All parse errors stored in the result are emitted as [[CompilerFailure]] diagnostics.
   * If the result carries no output value the computation is aborted immediately.
   *
   * @tparam A The type of the successfully parsed value.
   * @tparam S Additional effects carried by the incoming computation.
   * @param result The effectful parse result to lift.
   * @return The parsed value within the [[Compilation]] effect context, or an abort if
   *         the parse produced no output.
   */
  def fromParseResult[A, S](result: ParseResult[A] < S): A < (Compilation & S) =
    result.map(r =>
      Kyo.foreach(r.errors)(Emit.value)
        .andThen(
          r.out match
            case Absent         => Abort.fail(())
            case Present(value) => value
        )
    )

  /**
   * Execute a [[Compilation]] computation and collect the result.
   *
   * The provided `context` is injected into the environment, emitted failures are
   * accumulated, and the abort channel is folded into a [[kyo.Result]]:
   *   - [[kyo.Result.Success]] – the computation completed without failures.
   *   - [[kyo.Result.Failure]] – one or more [[CompilerFailure]] diagnostics were emitted.
   *   - [[kyo.Result.Panic]]   – an unexpected [[Throwable]] propagated out.
   *
   * @tparam A The type produced by a successful computation.
   * @tparam S Additional effects that remain after stripping [[Compilation]].
   * @param context The [[CompilationContext]] to make available to the computation.
   * @param body    The [[Compilation]] computation to execute.
   * @return A [[kyo.Result]] summarising the outcome, within the residual effect `S`.
   */
  def run[A, S](context: CompilationContext)(body: A < (Compilation & S))(using Frame): Result[Chunk[CompilerFailure], A] < (S & Sync) =
    body.handle(
      Env.run(context),
      Abort.fold[Unit](Result.Success[A](_), _ => Result.Failure(Chunk.empty), Result.Panic.apply),
      Emit.run(_).map((failures, result) =>
        if failures.isEmpty then result
        else Result.Failure(failures)
      )
    )

  /**
   * Emit a [[CompilerFailure]] diagnostic and then immediately abort the computation.
   *
   * This is a convenience combinator for the common pattern of recording a fatal
   * error and stopping further processing in a single step.
   *
   * @param failure The failure to record before aborting.
   * @return A value of type [[Nothing]] within the [[Compilation]] effect, ensuring
   *         the compiler sees the subsequent code as unreachable.
   */
  def emitAndAbort(failure: CompilerFailure): Nothing < Compilation =
    Emit.value(failure).andThen(Abort.fail(()))
