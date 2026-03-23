package io.github.iltotore.redhdl.parser

import kyo.*

/**
 * Wrap a parser with a debug trace that prints the parser name, the parse state
 * before and after, and any errors that were recorded.
 *
 * This is a development utility; it has no effect on the parse result but writes
 * diagnostic information to standard output when the parser is exercised.
 *
 * @tparam A  The value type produced by the parser.
 * @tparam In The input element type.
 * @param name   A human-readable name for the parser, printed in the trace output.
 * @param parser The parser to trace.
 * @return A parser that behaves identically to `parser` but emits trace output.
 */
def debug[A, In](name: String)(parser: A < Parse[In])(using Tag[In], Frame): A < Parse[In] =
  Parse.modifyState[A][In](state =>
    val (outState, result) = Parse.runState(state)(parser).eval
    val errorMsg =
      if outState.failures.isEmpty then ""
      else outState.failures.map(err => s"${err.position}: ${err.message}").mkString("\n- ", "\n- ", "")
    (outState, result.out)
  )

/**
 * Wrap a parser so that, when it fails to produce a value, a custom error message
 * is recorded at the position of the last attempt.
 *
 * The parser is first run speculatively via [[Parse.attempt]]; if it returns
 * [[kyo.Absent]] (i.e. it failed without consuming input), the supplied `message`
 * is appended to the failure list at the position captured before the attempt, and
 * [[kyo.Absent]] is propagated to the caller.
 *
 * @tparam A  The value type produced by the parser on success.
 * @tparam In The input element type.
 * @tparam S  Additional Kyo effects carried by the parser.
 * @param parser  The parser to augment with an error message.
 * @param message The error message to record when `parser` fails.
 * @return A parser equivalent to `parser` that additionally records `message` on
 *         failure at the position where the attempt began.
 */
def withErrorMessage[A, In, S](parser: A < (Parse[In] & S), message: String)(using Tag[In], Frame): A < (Parse[In] & S) =
  for
    snapshot <- Parse.modifyState[ParseState[In]][In](state => (state, Present(state)))
    result <- Parse.attempt(parser)
  yield result match
    case Absent => Parse.modifyState(state =>
        (state.copy(failures = snapshot.failures :+ ParseFailure(message, snapshot.input.position)), Absent)
      )
    case Present(value) => value
