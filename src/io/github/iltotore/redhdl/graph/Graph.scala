package io.github.iltotore.redhdl.graph

import kyo.*

case class Graph(nodes: Chunk[Node]):

  def getNode(id: NodeId): Maybe[Node] = 
    if nodes.sizeCompare(id.value) > 1 then Present(nodes(id.value))
    else Absent

  def withNode(node: Node): (Graph, NodeId) =
    val id = NodeId.assume(nodes.size)
    (this.copy(nodes = nodes.appended(node)), id)

object Graph:

  val empty: Graph = Graph(Chunk.empty)

  def modify(f: Graph => Graph < GraphBuilding): Unit < GraphBuilding =
    Var.use[Graph](f)
      .map(Var.set)
      .unit

  def modifyReturn[A](f: Graph => (Graph, A) < GraphBuilding): A < GraphBuilding =
    Var.use[Graph](f)
      .map(Var.set(_).andThen(_))

  def getNode(id: NodeId): Maybe[Node] < GraphBuilding =
    Var.use[Graph](_.getNode(id))

  def addNode(node: Node): NodeId < GraphBuilding =
    modifyReturn(_.withNode(node))