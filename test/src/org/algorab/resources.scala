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

object resources:

  def getResourcePath(url: URL): Path =
    if url.getProtocol == "file" then Paths.get(url.toURI)
    else
      val strings = url.toString.split("!")
      val jarFS = FileSystems.newFileSystem(URI.create(strings(0)), java.util.HashMap())
      jarFS.getPath(strings(1))

  def listResources(folder: String): Chunk[Path] =
    val path = getResourcePath(Main.getClass.getResource(folder))
    val ls = Files.list(path)
    Chunk.from(ls.collect(Collectors.toList()).asScala)

  def readResource(path: Path): String =
    Using.resource(Source.fromInputStream(Files.newInputStream(path), "UTF-8"))(_.mkString)

  def runGoldenTest(code: String): Unit =
    val result = compile(code)
    assert(!result.isFailure)

  transparent inline def goldenTests(): Unit =
    ${goldenTestsImpl()}

  def goldenTestsImpl()(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val cases: Chunk[Expr[Unit]] = listResources("/golden/good").map(file =>
      val fileName = Expr(file.getFileName().toString)
      val name = Expr(file.getFileName().toString().dropRight(5))
      '{
        test($fileName):
          val code = Using.resource(Source.fromInputStream(classOf[GoldenTests].getResourceAsStream("/golden/good/" + $fileName)))(_.mkString)
          runGoldenTest(code)
      }
    )

    Block(cases.map(_.asTerm).toList, '{()}.asTerm).asExprOf[Unit]