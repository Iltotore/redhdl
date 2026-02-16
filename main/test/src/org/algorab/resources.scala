package io.github.iltotore.redhdl

import java.net.URI
import java.net.URL
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.util.stream.Collectors
import scala.collection.JavaConverters.*
import kyo.*
import scala.quoted.*
import utest.*
import scala.util.Using
import scala.io.Source
import utest.asserts.Util
import io.github.iltotore.redhdl.graph.GraphRouter
import io.github.iltotore.redhdl.ast.Identifier

object resources:

  private val defaultContext: CompilationContext = CompilationContext(
    fileName = Absent,
    entrypoint = Absent,
    optimize = true
  )

  def runAssert[A](context: CompilationContext)(body: A < Compilation)(using Frame): Unit =
    import AllowUnsafe.embrace.danger
    Sync.Unsafe.evalOrThrow[Unit](
      Compilation.run(context)(body).map:
        case Result.Success(_) => Kyo.unit
        case result@Result.Failure(failures) =>
          val prettyFailures = failures.map(_.toPrettyString).mkString("- ", "\n- ", "")
          Util.assertError(s"Compilation failed:\n$prettyFailures", Seq(TestValue.Single("result", None, result)))
        case result@Result.Panic(error) => Util.assertError("Compilation panic", Seq(TestValue.Single("result", None, result)), error)
    )

  def getResourcePath(url: URL): Path =
    if url.getProtocol == "file" then Paths.get(url.toURI)
    else
      val strings = url.toString.split("!")
      val jarFS = FileSystems.newFileSystem(URI.create(strings(0)), java.util.HashMap())
      jarFS.getPath(strings(1))

  def listResources(folder: String): Chunk[Path] =
    val path = getResourcePath(this.getClass.getResource(folder))
    val ls = Files.walk(path).filter(Files.isRegularFile(_))
    Chunk.from(ls.map(path.relativize).collect(Collectors.toList()).asScala)

  def readResource(path: Path): String =
    Using.resource(Source.fromInputStream(Files.newInputStream(path), "UTF-8"))(_.mkString)

  def runGoldenTest(name: Identifier, code: String): Unit =
    runAssert(defaultContext)(
      parse(code)
        .map(typecheck)
        .map(components =>
              val componentName =
                components.collectFirst:
                  case (n, _) if n.value.equalsIgnoreCase(name.value) => n
                .getOrElse(Identifier("Main"))

              val initialGraph = compileToGraph(componentName, components)
              val initialLayers = GraphRouter.getLayers(initialGraph)
              GraphRouter.addRelays(initialGraph, initialLayers): Unit
        )
    )

  transparent inline def goldenTests(): Unit =
    ${goldenTestsImpl()}

  def goldenTestsImpl()(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val cases: Chunk[Expr[Unit]] = listResources("/golden/good").map(file =>
      val pathStr = Expr(file.toString)
      val name = Expr(file.toString().dropRight(4).replace("/", "."))
      val componentName = Expr(file.getFileName().toString().dropRight(4))
      '{
        test($name):
          val code = Using.resource(Source.fromInputStream(classOf[GoldenTests].getResourceAsStream("/golden/good/" + $pathStr)))(_.mkString)
          runGoldenTest(Identifier.assume($componentName), code)
      }
    )

    Block(cases.map(_.asTerm).toList, '{()}.asTerm).asExprOf[Unit]