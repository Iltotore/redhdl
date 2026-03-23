package io.github.iltotore.redhdl

import com.monovore.decline.*
import kyo.*

/**
 * A base class for command-line applications backed by [[https://getkyo.io Kyo]] effects.
 *
 * Wraps a [[com.monovore.decline.Command]] and runs it inside the Kyo runtime, handling
 * argument parsing errors by printing a human-readable usage message to stderr and
 * recovering from [[String]]-typed abort failures in the same way.
 *
 * @constructor Create an app from a fully-constructed Decline [[Command]].
 * @param command The Decline command to execute when the application starts.
 */
class KyoCommandApp(command: Command[Unit < (Async & Abort[String | Throwable] & Scope)]) extends KyoApp:

  /**
   * Convenience constructor that builds the [[Command]] from individual components.
   *
   * An optional `--version` flag is added when `version` is non-empty; it prints the
   * version string to stderr and exits.
   *
   * @param name      The name of the executable shown in help output.
   * @param header    A short description printed at the top of the help message.
   * @param main      The [[Opts]] describing the program's options and arguments.
   * @param helpFlag  Whether to include the standard `--help` flag (default `true`).
   * @param version   An optional version string; when non-empty a `--version` flag is added.
   */
  def this(
      name: String,
      header: String,
      main: Opts[Unit < (Async & Abort[String | Throwable] & Scope)],
      helpFlag: Boolean = true,
      version: String = ""
  ) =
    this {
      val showVersion =
        if (version.isEmpty) Opts.never
        else
          Opts
            .flag("version", "Print the version number and exit.", visibility = Visibility.Partial)
            .map(_ => Console.printLineErr(version))

      Command(name, header, helpFlag)(showVersion.orElse(main))
    }

  /**
   * Entry point called by the Kyo runtime.
   *
   * Parses the ambient command-line arguments using [[com.monovore.decline.Command.parse]].
   * On parse failure the error details and usage instructions are written to stderr.
   * On success the parsed Kyo effect is executed, with any [[String]] abort recovered
   * as an error message written to stderr.
   */
  run:
    command.parse(PlatformApp.ambientArgs getOrElse args, sys.env) match
      case Left(help) => Console.printLineErr(
          s"""Errors:
             |- ${help.errors.mkString("\n- ")}
             |
             |Usages:
             |- ${help.usage.map(usage => s"${help.prefix.toList.mkString("|")} $usage").mkString("\n- ")}
             |
             |${help.body.mkString("\n\n")}""".stripMargin
        )
      case Right(program) => Abort.recover[String](Console.printLineErr)(program)
