package io.github.iltotore.redhdl

import kyo.*
import scala.util.Using
import scala.io.Source
import io.github.iltotore.redhdl.ast.Identifier

object Main extends KyoApp:

  /* 
  ("out1", InputCall(identifier = Sub(subComponent = "id1", name = "out")))
   */

  run:
    direct:
      val code = Using.resource(Source.fromFile("test/resources/golden/good/and.red"))(_.mkString)

      val typeResult = typecheck(code)
      Console.printLine(typeResult).now
      Console.printLine("=" * 30).now

      typeResult match
        case Result.Success(components) =>
          val graph = compileToGraph(Identifier("And"), components)
          Console.printLine(graph).now
          Console.printLine(compileToSchem(graph)).now
        case _ => Console.printLine(typeResult).now
    