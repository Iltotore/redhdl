package io.github.iltotore.redhdl

import kyo.*
import com.monovore.decline.*

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
      case Left(help) => Console.printLineErr(help)
      case Right(program) => Abort.recover[String](Console.printLineErr)(program)