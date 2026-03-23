package io.github.iltotore.redhdl.ast

import io.github.iltotore.redhdl.ast.Component
import kyo.Chunk

/**
 * The top-level AST node produced by the parser.
 *
 * A [[Program]] is a flat, ordered collection of [[Component]] declarations.
 * Components may reference each other by name; the entry-point component is
 * selected later during compilation via the [[io.github.iltotore.redhdl.CompilationContext]].
 *
 * @param components All component declarations present in the source file, in
 *                   the order they appear.
 */
case class Program(components: Chunk[Component])
