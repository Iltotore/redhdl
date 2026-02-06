package io.github.iltotore.redhdl

import io.github.iltotore.redhdl.typer.TypeFailure
import kyo.ParseFailure

type CompilerFailure = ParseFailure | TypeFailure
