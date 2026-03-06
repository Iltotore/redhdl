package io.github.iltotore.redhdl

import com.monovore.decline.*
import kyo.*

class KyoCommandApp(command: Command[Unit < (Async & Abort[String | Throwable] & Scope)]) extends KyoApp:

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

  run:
    command.parse(PlatformApp.ambientArgs getOrElse args, sys.env) match
      case Left(help)     => Console.printLineErr(
        s"""Errors:
          |- ${help.errors.mkString("\n- ")}
          |
          |Usages:
          |- ${help.usage.map(usage => s"${help.prefix.toList.mkString("|")} $usage").mkString("\n- ")}
          |
          |${help.body.mkString("\n\n")}""".stripMargin
      )
      case Right(program) => Abort.recover[String](Console.printLineErr)(program)
