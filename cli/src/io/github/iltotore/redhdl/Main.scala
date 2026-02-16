package io.github.iltotore.redhdl

import cats.syntax.all.*
import com.monovore.decline.*
import kyo.*
import java.io.File
import io.github.iltotore.iron.decline.given
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.minecraft.Structure
import io.github.ensgijs.nbt.io.BinaryNbtHelpers
import io.github.ensgijs.nbt.io.CompressionType

given Argument[Path] = Argument.from("path")(str => Path(str).validNel)

extension [A](opts: Opts[A])
  def orAbsent: Opts[Maybe[A]] = opts.map(Present.apply).withDefault(Absent)

def programName(path: Path, extension: String): String = path.path match
  case _ :+ s"$name.$extension" => name
  case _ :+ name => name
  case _ => throw AssertionError("Empty path")

object Main extends KyoCommandApp(
  name = "redhdl",
  header = "Compile RedHDL file to schematic",
  main =
    (
      Opts.argument[Path]("path"),
      Opts.option[Path]("output", "Path to write the schematic to", "o").orAbsent,
      Opts.option[Identifier]("entrypoint", "Program entrypoint", "e").orAbsent,
      Opts.flag("no-optimize", "Disable optimizations").orTrue
    ).mapN((input, outputOpt, entrypoint, optimize) =>
      for
        exists <- input.exists
        _ <-
          if exists then Kyo.unit
          else Abort.fail(s"Path ${input.path.mkString(File.separator)} does not exist")
          
        inputIsDir <- input.isDir
        _ <-
          if inputIsDir then Abort.fail(s"Path ${input.path.mkString(File.separator)} is a directory")
          else Kyo.unit

        name = programName(input, "red")

        output = outputOpt.getOrElse(Path(s"$name.schem"))

        outputIsDir <- output.isDir
        _ <-
          if outputIsDir then Abort.fail(s"Path ${output.path.mkString(File.separator)} is a directory")
          else Kyo.unit

        context = CompilationContext(
          fileName = Present(name),
          entrypoint = entrypoint,
          optimize = optimize
        )

        _ <- Console.printLine(s"Compiling ${input.path.mkString(File.separator)} to ${output.path.mkString(File.separator)}")

        code <- input.read

        structure <- Compilation.run(context)(compileRedHDL(code)).map:
          case Result.Success(result) => result
          case Result.Failure(failures) =>
            val prettyFailures = failures
              .map(_.toPrettyString)
              .mkString("- ", "\n- ", "")
              
            Abort.fail(s"${failures.size} Compilation failures:\n$prettyFailures")

          case Result.Panic(throwable) => Abort.panic(throwable)

        tag = Structure.saveSponge(structure)
      yield
        BinaryNbtHelpers.write(tag, output.path.mkString(File.separator), CompressionType.GZIP): Unit
    )
)