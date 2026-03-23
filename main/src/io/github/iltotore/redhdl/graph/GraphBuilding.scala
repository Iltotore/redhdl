package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ir.SimplifiedComponent
import kyo.*

/**
 * The Kyo effect type used while constructing a [[Graph]].
 *
 * Graph-building computations carry a mutable [[Graph]] state via [[kyo.Var]].
 * All graph-mutation helpers in [[Graph]] and [[GraphBuilder]] operate within
 * this effect.
 */
type GraphBuilding = Var[Graph]

/**
 * Runners and entry points for [[GraphBuilding]] computations.
 */
object GraphBuilding:

  /**
   * Execute a [[GraphBuilding]] computation starting from an empty graph whose
   * input nodes correspond to the given identifiers.
   *
   * @tparam A The value type produced by the computation.
   * @tparam S Additional effects carried by the computation.
   * @param inputs The ordered list of input port names used to initialise the graph.
   * @param body   The graph-building computation to run.
   * @return A pair of the final [[Graph]] state and the value returned by `body`.
   */
  def run[A, S](inputs: Chunk[Identifier])(body: A < (GraphBuilding & S)): (Graph, A) < S =
    Var.runTuple(Graph.fromInputs(inputs))(body)

  /**
   * Compile a [[SimplifiedComponent]] to a logic [[Graph]].
   *
   * Initialises the graph with the component's input ports and then calls
   * [[GraphBuilder.buildOutputsGraph]] to populate the output driver logic.
   *
   * @param component The flattened, simplified component to translate.
   * @return The complete [[Graph]] for the component.
   */
  def buildGraph(component: SimplifiedComponent): Graph =
    run(component.inputs)(GraphBuilder.buildOutputsGraph(component.outputs)).eval._1
