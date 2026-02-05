package io.github.iltotore.redhdl.graph

import io.github.iltotore.redhdl.ast.Identifier
import kyo.*

case class Graph(inputs: Map[Identifier, NodeId], nodes: Chunk[Node]):

  def getInputId(name: Identifier): NodeId =
    inputs.getOrElse(name, throw AssertionError(s"Use of unknown input: $name"))

  def getNode(id: NodeId): Node =
    if nodes.sizeCompare(id.value) > 1 then throw AssertionError(s"Use of unknown node: $id")
    else nodes(id.value)

  def withNode(node: Node): (Graph, NodeId) =
    val id = NodeId.assume(nodes.size)
    (this.copy(nodes = nodes.appended(node)), id)

  def modifyNode(id: NodeId, f: Node => Node): Graph =
    this.copy(nodes = nodes.updated(id.value, f(getNode(id))))

  def getOutputs(id: NodeId): Chunk[NodeOutput] = getNode(id).outputs

object Graph:

  def fromInputs(inputs: Chunk[Identifier]): Graph =
    Graph(
      NodeId.assumeAll(inputs.zipWithIndex.toMap),
      inputs.map(input => Node(NodeType.Input(input), Chunk.empty))
    )

  def modify(f: Graph => Graph < GraphBuilding): Unit < GraphBuilding =
    Var.use[Graph](f)
      .map(Var.set)
      .unit

  def modifyReturn[A](f: Graph => (Graph, A) < GraphBuilding): A < GraphBuilding =
    Var.use[Graph](f)
      .map(Var.set(_).andThen(_))

  def getInputId(name: Identifier): NodeId < GraphBuilding =
    Var.use[Graph](_.getInputId(name))

  def getNode(id: NodeId): Node < GraphBuilding =
    Var.use[Graph](_.getNode(id))

  def addNode(node: Node): NodeId < GraphBuilding =
    modifyReturn(_.withNode(node))

  def addOutput(input: NodeId, output: NodeOutput): Unit < GraphBuilding =
    modify(_.modifyNode(input, _.withOutput(output)))

  def createOutput(output: NodeOutput, nodeType: NodeType): NodeId < GraphBuilding =
    addNode(Node(nodeType, Chunk.empty)).map(id => addOutput(id, output).andThen(id))
