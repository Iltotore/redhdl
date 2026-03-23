package io.github.iltotore.redhdl.ast

/**
 * The set of types understood by the RedHDL type system.
 *
 * RedHDL is currently a single-type language: every port carries a one-bit
 * Boolean signal.  The `derives CanEqual` clause enables structural equality
 * comparisons, which is used by the type checker when asserting that the
 * type of an expression matches the expected port type.
 */
enum Type derives CanEqual:
  /** The single Boolean type, representing a one-bit digital signal. */
  case Bool
