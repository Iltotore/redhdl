package io.github.iltotore.redhdl

import cats.syntax.all.*
import com.monovore.decline.*
import io.github.iltotore.iron.decline.given
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.minecraft.Palette
import io.github.iltotore.redhdl.minecraft.RepeaterDelay
import io.github.iltotore.redhdl.minecraft.Structure
import io.github.iltotore.redhdl.minecraft.nbt.NBT
import java.io.File
import java.io.FileOutputStream
import kyo.*

/**
 * Decline [[Argument]] instance for filesystem [[Path]] values.
 *
 * Accepts any non-empty string and wraps it in a [[Path]] without additional
 * validation; path existence checks are performed at runtime.
 */
given Argument[Path] = Argument.from("path")(str => Path(str).validNel)

extension [A](opts: Opts[A])
  /**
   * Makes an option optional, returning [[Absent]] when the flag is not provided.
   *
   * @return An [[Opts]] that yields [[Present]] when the user supplies the flag
   *         and [[Absent]] otherwise.
   */
  def orAbsent: Opts[Maybe[A]] = opts.map(Present.apply).withDefault(Absent)

/**
 * Derive a program name from a file path by stripping the directory part and
 * the given extension.
 *
 * @param path      The file path to extract the name from.
 * @param extension The file extension to strip (without the leading dot).
 * @return The base name of the file, without directory or extension.
 * @throws AssertionError if `path` represents an empty sequence of segments.
 */
def programName(path: Path, extension: String): String = path.path match
  case _ :+ s"$name.$extension" => name
  case _ :+ name                => name
  case _                        => throw AssertionError("Empty path")

/**
 * Main entry point for the `redhdl` command-line tool.
 *
 * Compiles a RedHDL source file to a Minecraft Sponge schematic (`.schem`).
 *
 * ==Usage==
 * {{{
 *   redhdl <path> [options]
 * }}}
 *
 * ==Options==
 *   - `path`               – Path to the `.red` source file (required).
 *   - `--output / -o`      – Destination path for the schematic (default: `<name>.schem`).
 *   - `--entrypoint / -e`  – Component to use as the circuit entry point.
 *   - `--no-optimize`      – Disable constant-folding and algebraic optimisations.
 *   - `--no-align`         – Disable vertical alignment of output nodes.
 *   - `--palette`          – Block ID(s) for wires; the special value `rainbow` expands to the
 *                            full set of coloured wools.
 *   - `--repeater-delay`   – Redstone repeater delay (1–4, default 1).
 */
object Main extends KyoCommandApp(
      name = "redhdl",
      header = "Compile RedHDL file to schematic",
      version = "1.0.0",
      main =
        (
          Opts.argument[Path]("path"),
          Opts.option[Path]("output", "Path to write the schematic to", "o").orAbsent,
          Opts.option[Identifier]("entrypoint", "Program entrypoint", "e").orAbsent,
          Opts.flag("no-optimize", "Disable optimizations").orTrue,
          Opts.flag("no-align", "Disable output nodes alignment").orTrue,
          Opts.options[String]("palette", "Block id to use for wires or alias (rainbow)").orEmpty,
          Opts.option[RepeaterDelay]("repeater-delay", "Set the repeater delay between 1 (fastest) and 4 (longest)").withDefault(RepeaterDelay(1))
        ).mapN((input, outputOpt, entrypoint, optimize, alignOutputs, paletteStrs, repeaterDelay) =>
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
              optimize = optimize,
              alignOutputs = alignOutputs,
              palette = Palette.fromStrings(Chunk.from(paletteStrs)),
              repeaterDelay = repeaterDelay
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
          yield tag.writeGZipped("", FileOutputStream(output.path.mkString(File.separator))): Unit
        )
    )
