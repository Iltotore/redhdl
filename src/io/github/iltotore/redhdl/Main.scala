package io.github.iltotore.redhdl

import kyo.*
import scala.util.Using
import scala.io.Source
import io.github.iltotore.redhdl.ast.Identifier

object Main extends KyoApp:

  run:
    direct:
      val code = Using.resource(Source.fromFile("test/resources/golden/good/subcomponent.red"))(_.mkString)

      val typeResult = typecheck(code)
      Console.printLine(typeResult).now
      Console.printLine("=" * 30).now

      typeResult match
        case Result.Success(components) =>
          Console.printLine(compile(Identifier("BiIdentity"), components)).now
        case _ => Console.printLine(typeResult).now
    