package io.github.iltotore.redhdl.typer

import io.github.iltotore.redhdl.Compilation
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ast.Program
import kyo.*

/**
 * The base Kyo effect type for the RedHDL type-checking pass.
 *
 * A computation of type `A < Typing` may:
 *   - Emit zero or more [[TypeFailure]] diagnostics via [[kyo.Emit]].
 *   - Abort the current computation early (on a fatal type error) via
 *     [[kyo.Abort]][Unit].
 */
type Typing = Emit[TypeFailure] & Abort[Unit]

/**
 * Type aliases and runners for [[Typing]] computations.
 */
object Typing:

  /**
   * The effect type used during program-level type checking.
   *
   * Extends [[Typing]] with a mutable [[GlobalContext]] variable that tracks all
   * component declarations seen so far.
   */
  type Global = Var[GlobalContext] & Typing

  /**
   * The effect type used during body-level type checking of a single component.
   *
   * Extends [[Typing]] with a mutable [[ComponentContext]] variable that tracks the
   * current component's port declarations and assignment progress.
   */
  type Component = Var[ComponentContext] & Typing

  /**
   * Run a [[Typing.Global]] computation and return the resulting component map.
   *
   * The [[GlobalContext]] is initialised with the [[GlobalContext.default]] built-in
   * declarations.  All emitted [[TypeFailure]]s and the abort channel are forwarded
   * as [[io.github.iltotore.redhdl.CompilerFailure]] diagnostics into the ambient
   * [[Compilation]] effect.
   *
   * @tparam S Additional effects carried by the computation.
   * @param body The type-checking computation to run.
   * @return A map from component [[Identifier]] to [[ComponentInfo]] for every
   *         successfully checked component, within the [[Compilation]] effect.
   */
  def runGlobal[S](body: Unit < (Typing.Global & S)): Map[Identifier, ComponentInfo] < (Compilation & S) =
    Var.runTuple(GlobalContext.default)(body).map((ctx, _) => ctx.components)

  /**
   * Emit a [[TypeFailure]] diagnostic without aborting the computation.
   *
   * Use this for non-fatal errors where checking can continue to discover further
   * violations.
   *
   * @param failure The failure to emit.
   */
  def fail(failure: TypeFailure): Unit < Typing = Emit.value(failure)

  /**
   * Emit a [[TypeFailure]] diagnostic and then immediately abort the computation.
   *
   * Use this for fatal errors where it is not meaningful to continue (e.g. when a
   * port type cannot be determined and all subsequent checks would be unsound).
   *
   * @param failure The failure to emit before aborting.
   * @return A value of type [[Nothing]], signalling that subsequent code is unreachable.
   */
  def failAndAbort(failure: TypeFailure): Nothing < Typing =
    fail(failure).andThen(Abort.fail(()))

  /**
   * Run `body` so that the computation is aborted as soon as any [[TypeFailure]] is
   * emitted, while still forwarding the failure to the outer emit handler.
   *
   * @tparam A The value type produced by `body` on success.
   * @param body The computation to run under the abort-on-emit policy.
   * @return The value produced by `body` if no failures were emitted.
   */
  def abortIfFail[A](body: A < Typing): A < Typing =
    abortIfEmit.run(body)

  /**
   * An [[Isolate]] that runs an inner [[Emit]][TypeFailure] channel and aborts the
   * computation if any failure was emitted when the inner scope exits.
   *
   * This is the mechanism behind [[abortIfFail]]: the inner failures are collected,
   * re-emitted to the outer handler, and then an [[Abort]] is triggered if the
   * collected set is non-empty.
   */
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
