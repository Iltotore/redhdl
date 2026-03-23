package io.github.iltotore.redhdl.ir

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.typer.ComponentInfo
import kyo.*

/**
 * The Kyo effect type that provides the component lookup environment used during
 * sub-component expansion.
 *
 * [[Expansion]] is a [[kyo.Env]] carrying a map from component [[Identifier]] to
 * [[io.github.iltotore.redhdl.typer.ComponentInfo]].  It is threaded through
 * [[Expander.expandComponent]] so that nested sub-component types can be resolved
 * without passing the map explicitly as a parameter.
 */
type Expansion = Env[Map[Identifier, ComponentInfo]]

/**
 * Utilities for running [[Expansion]] computations and reading component definitions
 * from the environment.
 */
object Expansion:

  /**
   * Provide the component map to an [[Expansion]] computation, eliminating the
   * [[Expansion]] effect from its type.
   *
   * @tparam A The value type produced by the computation.
   * @tparam S Additional effects carried by the computation.
   * @param env  The component lookup map to inject.
   * @param body The computation that requires the [[Expansion]] environment.
   * @return The result of `body` with the [[Expansion]] effect eliminated.
   */
  def run[A, S](env: Map[Identifier, ComponentInfo])(body: A < (Expansion & S)): A < S =
    Env.run(env)(body)

  /**
   * Look up a component by name in the ambient [[Expansion]] environment.
   *
   * @param identifier The name of the component to retrieve.
   * @return The [[ComponentInfo]] for `identifier` within the [[Expansion]] effect.
   * @throws NoSuchElementException if `identifier` is not present in the map,
   *                                indicating a type-checker bug.
   */
  def getComponent(identifier: Identifier): ComponentInfo < Expansion =
    Env.use[Map[Identifier, ComponentInfo]](_(identifier))
