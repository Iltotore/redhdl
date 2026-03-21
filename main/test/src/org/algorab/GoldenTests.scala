package io.github.iltotore.redhdl

import kyo.*
import java.io.IOException
import scala.io.Source
import java.nio.file.Files
import utest.*
import scala.quoted.*
import scala.annotation.nowarn

/**
 * Golden-file test suite for the RedHDL compiler.
 *
 * Each `.red` file found under `resources/golden/good/` is compiled end-to-end
 * (up to graph construction) and expected to succeed without any [[CompilerFailure]]
 * diagnostics.  New golden tests are added simply by placing a valid `.red` file in
 * that directory; the macro [[resources.goldenTests]] discovers them at compile time
 * and generates a utest `test` block for each one.
 *
 * @see [[resources.goldenTests]] for the macro that expands the test cases.
 * @see [[resources.runGoldenTest]] for the actual assertion logic.
 */
class GoldenTests extends TestSuite:

  /** The dynamically generated collection of golden test cases. */
  @nowarn("msg=pure")
  val tests = Tests:
    resources.goldenTests()
