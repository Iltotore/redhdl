package io.github.iltotore.redhdl

import kyo.*

object Main extends KyoApp:

  run:
    val code = """component And
                 |begin
                 |end""".stripMargin

    // Console.printLine(code.substring(167))
    Console.printLine(parse(code))