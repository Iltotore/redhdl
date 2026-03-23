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
import io.github.iltotore.redhdl.minecraft.Palette
import io.github.iltotore.redhdl.minecraft.RepeaterDelay

/**
 * Shared test infrastructure and macro utilities for the golden-file test suite.
 *
 * This object provides:
 *   - A [[defaultContext]] compilation context used by all golden tests.
 *   - [[runAssert]] for executing a [[Compilation]] body and asserting success.
 *   - Resource-path helpers ([[getResourcePath]], [[listResources]], [[readResource]]).
 *   - [[runGoldenTest]] for compiling a single `.red` file up to graph construction.
 *   - [[goldenTests]] / [[goldenTestsImpl]], a compile-time macro that discovers all
 *     golden test files and generates a utest `test` block for each one.
 */
object resources:

  /**
   * The default [[CompilationContext]] used by golden tests.
   *
   * No explicit file name or entry-point is set; optimisations and output alignment
   * are both enabled; the default palette and a delay of 1 are used.
   */
  private val defaultContext: CompilationContext = CompilationContext(
    fileName = Absent,
    entrypoint = Absent,
    optimize = true,
    alignOutputs = true,
    palette = Palette.default,
    repeaterDelay = RepeaterDelay(1)
  )

  /**
   * Execute a [[Compilation]] body within `context` and assert that it succeeds
   * without any [[CompilerFailure]] diagnostics.
   *
   * On failure, a utest assertion error is thrown that includes the pretty-printed
   * failure messages.  On panic, the underlying [[Throwable]] is re-thrown.
   *
   * @tparam A The value type produced by `body` (ignored on success).
   * @param context The [[CompilationContext]] to inject.
   * @param body    The [[Compilation]] computation to validate.
   */
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

  /**
   * Resolve a classpath resource [[URL]] to a [[java.nio.file.Path]].
   *
   * Handles both plain `file://` URLs (typical in development) and `jar:file://`
   * URLs (typical when running from a packaged JAR) by creating a new
   * [[java.nio.file.FileSystem]] for the JAR if necessary.
   *
   * @param url The URL to resolve.
   * @return The corresponding [[java.nio.file.Path]].
   */
  def getResourcePath(url: URL): Path =
    if url.getProtocol == "file" then Paths.get(url.toURI)
    else
      val strings = url.toString.split("!")
      val jarFS = FileSystems.newFileSystem(URI.create(strings(0)), java.util.HashMap())
      jarFS.getPath(strings(1))

  /**
   * List all regular files under a classpath resource folder, relative to the folder
   * root.
   *
   * @param folder The classpath-relative folder path (e.g. `"/golden/good"`).
   * @return A [[kyo.Chunk]] of relative [[java.nio.file.Path]]s for every regular file
   *         found in the folder tree.
   */
  def listResources(folder: String): Chunk[Path] =
    val path = getResourcePath(this.getClass.getResource(folder))
    val ls = Files.walk(path).filter(Files.isRegularFile(_))
    Chunk.from(ls.map(path.relativize).collect(Collectors.toList()).asScala)

  /**
   * Read the entire contents of a file as a [[String]] using UTF-8 encoding.
   *
   * @param path The file to read.
   * @return The file contents as a string.
   */
  def readResource(path: Path): String =
    Using.resource(Source.fromInputStream(Files.newInputStream(path), "UTF-8"))(_.mkString)

  /**
   * Compile a single golden test source file up to graph construction and assert
   * that it succeeds.
   *
   * The test parses the source, type-checks it, resolves the entry-point component
   * by matching the file name (case-insensitively), builds the graph, and calls
   * [[GraphRouter.addRelays]].  Schematic generation is not exercised here.
   *
   * @param name The component name to use as the entry point (derived from the file name).
   * @param code The source text of the `.red` file.
   */
  def runGoldenTest(name: Identifier, code: String): Unit =
    runAssert(defaultContext)(
      for
        components <- parse(code).map(typecheck)
        optimize <- CompilationContext.optimize
        alignOutputs <- CompilationContext.alignOutputs
      yield
        val componentName =
          components.collectFirst:
            case (n, _) if n.value.equalsIgnoreCase(name.value) => n
          .getOrElse(Identifier("Main"))

        val initialGraph = compileToGraph(componentName, components, true)
        val initialLayers = GraphRouter.getLayers(initialGraph, alignOutputs)
        GraphRouter.addRelays(initialGraph, initialLayers): Unit
    )

  /**
   * Inline entry point that expands to a sequence of utest `test` blocks, one for
   * each `.red` file discovered under `resources/golden/good/` at compile time.
   *
   * This macro is called inside the `Tests { … }` block of [[GoldenTests]].
   *
   * @return A [[scala.Unit]] expression containing the expanded test definitions.
   */
  transparent inline def goldenTests(): Unit =
    ${goldenTestsImpl()}

  /**
   * Compile-time implementation of the [[goldenTests]] macro.
   *
   * Discovers all resource files under `/golden/good/` at the time of compilation,
   * and for each file generates a utest `test(name) { … }` expression that reads the
   * file from the classpath and calls [[runGoldenTest]].
   *
   * @param Quotes Implicit quotes context provided by the Scala 3 macro engine.
   * @return A [[scala.quoted.Expr]][Unit] containing all generated `test` blocks,
   *         joined in a single [[scala.quoted.quotes.reflect.Block]].
   */
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
