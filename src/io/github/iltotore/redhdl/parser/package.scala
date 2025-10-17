package io.github.iltotore.redhdl.parser

import kyo.*

def debug[A, In](name: String)(parser: A < Parse[In])(using Tag[In], Frame): A < Parse[In] =
    Parse.modifyState[A][In](state =>
      println(s"$name start ${state.input.position}")
      val (outState, result) = Parse.runState(state)(parser).eval
      val errorMsg =
        if outState.failures.isEmpty then ""
        else outState.failures.map(err => s"${err.position}: ${err.message}").mkString("\n- ", "\n- ", "")
      println(s"$name end ${outState.input.position}: $result$errorMsg")
      (outState, result.out)
    )

def withErrorMessage[A, In, S](parser: A < (Parse[In] & S), message: String)(using Tag[In], Frame): A < (Parse[In] & S) =
  for
    snapshot <- Parse.modifyState[ParseState[In]][In](state => (state, Present(state)))
    result <- Parse.attempt(parser)
  yield
    result match
      case Absent => Parse.modifyState(state =>
        (state.copy(failures = snapshot.failures :+ ParseFailure(message, snapshot.input.position)), Absent)  
      )
      case Present(value) => value