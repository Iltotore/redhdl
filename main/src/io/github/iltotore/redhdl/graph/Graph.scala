package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Identifier
import kyo.*

/**
 * An immutable directed acyclic graph of logic nodes.
 *
 * Each [[Node]] in the graph represents a logic gate or an I/O pin, and each
 * [[NodeOutput]] edge records which input slot of a downstream node is driven
 * by the upstream node.
 *
 * Input nodes are special: they are identified by name and do not have incoming
 * edges.  They are pre-allocated when the graph is created via
 * [[Graph.fromInputs]].
 *
 * @param inputs A map from [[Identifier]] to [[NodeId]] for the named input nodes.
 * @param nodes  All nodes in the graph, addressable by their zero-based [[NodeId]].
 */
case class Graph(inputs: Map[Identifier, NodeId], nodes: Chunk[Node]):

  /**
   * Look up the [[NodeId]] of a named input.
   *
   * @param name The input port name.
   * @return The [[NodeId]] of the input node.
   * @throws AssertionError if `name` is not a known input.
   */
  def getInputId(name: Identifier): NodeId =
    inputs.getOrElse(name, throw AssertionError(s"Use of unknown input: $name"))

  /**
   * Retrieve a node by its identifier.
   *
   * @param id The [[NodeId]] to look up.
   * @return The corresponding [[Node]].
   * @throws AssertionError if `id` is out of bounds.
   */
  def getNode(id: NodeId): Node =
    if nodes.sizeCompare(id.value) > 1 then throw AssertionError(s"Use of unknown node: $id")
    else nodes(id.value)

  /**
   * Append a new node to the graph.
   *
   * @param node The [[Node]] to add.
   * @return A pair of the updated graph and the [[NodeId]] assigned to the new node.
   */
  def withNode(node: Node): (Graph, NodeId) =
    val id = NodeId.assume(nodes.size)
    (this.copy(nodes = nodes.appended(node)), id)

  /**
   * Apply a transformation to the node at the given identifier.
   *
   * @param id The [[NodeId]] of the node to modify.
   * @param f  The transformation function.
   * @return A new [[Graph]] with the node replaced by `f(node)`.
   */
  def modifyNode(id: NodeId, f: Node => Node): Graph =
    this.copy(nodes = nodes.updated(id.value, f(getNode(id))))

  /**
   * Return all outgoing edges from the node at the given identifier.
   *
   * @param id The [[NodeId]] to query.
   * @return The chunk of [[NodeOutput]] edges originating at `id`.
   */
  def getOutputs(id: NodeId): Chunk[NodeOutput] = getNode(id).outputs

  /**
   * Determine whether the node at the given identifier is an output node.
   *
   * @param id The [[NodeId]] to test.
   * @return `true` if the node's type is [[NodeType.Output]].
   */
  def isOutputNode(id: NodeId): Boolean = getNode(id).tpe match
    case NodeType.Output(_) => true
    case _                  => false

/**
 * Factory and stateful helpers for building and modifying a [[Graph]] inside a
 * [[GraphBuilding]] effect.
 */
object Graph:

  /**
   * Create an initial [[Graph]] with one input node per supplied identifier.
   *
   * The nodes are created in order so that the `i`-th identifier maps to
   * [[NodeId]] `i`.
   *
   * @param inputs The ordered list of input port names.
   * @return A [[Graph]] pre-populated with the given input nodes.
   */
  def fromInputs(inputs: Chunk[Identifier]): Graph =
    Graph(
      NodeId.assumeAll(inputs.zipWithIndex.toMap),
      inputs.map(input => Node(NodeType.Input(input), Chunk.empty))
    )

  /**
   * Apply a stateful graph transformation inside a [[GraphBuilding]] computation.
   *
   * @param f A function from the current [[Graph]] to an effectful updated [[Graph]].
   */
  def modify(f: Graph => Graph < GraphBuilding): Unit < GraphBuilding =
    Var.use[Graph](f)
      .map(Var.set)
      .unit

  /**
   * Apply a stateful graph transformation that also produces a result value.
   *
   * @tparam A The type of the extra value produced by `f`.
   * @param f  A function that returns both an updated [[Graph]] and a result.
   * @return The result produced by `f` after installing the updated graph.
   */
  def modifyReturn[A](f: Graph => (Graph, A) < GraphBuilding): A < GraphBuilding =
    Var.use[Graph](f)
      .map(Var.set(_).andThen(_))

  /**
   * Read the [[NodeId]] of a named input from the current graph state.
   *
   * @param name The input port name.
   * @return The [[NodeId]] of the input node.
   */
  def getInputId(name: Identifier): NodeId < GraphBuilding =
    Var.use[Graph](_.getInputId(name))

  /**
   * Read a node from the current graph state.
   *
   * @param id The [[NodeId]] to look up.
   * @return The [[Node]] corresponding to `id`.
   */
  def getNode(id: NodeId): Node < GraphBuilding =
    Var.use[Graph](_.getNode(id))

  /**
   * Append a new node to the current graph and return its assigned [[NodeId]].
   *
   * @param node The [[Node]] to add.
   * @return The [[NodeId]] assigned to the new node.
   */
  def addNode(node: Node): NodeId < GraphBuilding =
    modifyReturn(_.withNode(node))

  /**
   * Register an outgoing edge on an existing node.
   *
   * @param input  The [[NodeId]] of the source node.
   * @param output The [[NodeOutput]] describing the edge to add.
   */
  def addOutput(input: NodeId, output: NodeOutput): Unit < GraphBuilding =
    modify(_.modifyNode(input, _.withOutput(output)))

  /**
   * Add a new node of the given type and immediately wire `output` as its outgoing edge.
   *
   * This is a convenience combination of [[addNode]] and [[addOutput]].
   *
   * @param output   The [[NodeOutput]] edge that the new node should drive.
   * @param nodeType The [[NodeType]] of the new node.
   * @return The [[NodeId]] of the newly created node.
   */
  def createOutput(output: NodeOutput, nodeType: NodeType): NodeId < GraphBuilding =
    addNode(Node(nodeType, Chunk.empty)).map(id => addOutput(id, output).andThen(id))
