package io.github.iltotore.redhdl

import kyo.*
import java.io.IOException
import scala.io.Source
import java.nio.file.Files
import utest.*
import scala.quoted.*
import scala.annotation.nowarn

class GoldenTests extends TestSuite:

  @nowarn("msg=pure")
  val tests = Tests:
    resources.goldenTests()