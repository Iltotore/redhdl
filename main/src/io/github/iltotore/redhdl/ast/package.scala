package io.github.iltotore.redhdl.ast

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

/**
 * A refined [[String]] type representing a valid RedHDL identifier.
 *
 * An `Identifier` is a non-blank string (i.e. it must contain at least one
 * non-whitespace character).  The refinement is enforced by the
 * [[io.github.iltotore.iron]] library via the [[io.github.iltotore.iron.constraint.all.Not]]`[`[[io.github.iltotore.iron.constraint.all.Blank]]`]` constraint.
 *
 * Use `Identifier(str)` for checked construction at compile time, and
 * `Identifier.assume(str)` for unchecked construction in contexts where the
 * invariant is already guaranteed by the program logic.
 */
type Identifier = Identifier.T

/**
 * Companion object for the [[Identifier]] refined type.
 *
 * Extends [[io.github.iltotore.iron.RefinedType]] to provide smart constructors
 * and the standard Iron utilities (`assume`, `assumeAll`, etc.).
 *
 * A [[CanEqual]] instance is derived so that identifiers can be compared with
 * `==` without accidental widening to unrefined `String`.
 */
object Identifier extends RefinedType[String, Not[Blank]]:

  /** Enables structural equality between two [[Identifier]] values. */
  given CanEqual[Identifier, Identifier] = CanEqual.derived
