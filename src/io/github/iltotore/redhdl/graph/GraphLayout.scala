package io.github.iltotore.redhdl.graph

import kyo.Chunk
import scala.collection.mutable

object GraphLayout:

  /**
   * Get graph's layers using Kahn's algorithm.
   * 
   * @param graph the graph to sort
   * @return the graph's topological layers
   */
  def getLayers(graph: Graph): Chunk[Chunk[NodeId]] =
    val inputDegree = mutable.Map[NodeId, Int]().withDefaultValue(0)

    for (node, id) <- graph.nodes.zipWithIndex do
      inputDegree.getOrElseUpdate(NodeId.assume(id), 0): Unit
      node.outputs.foreach(output => inputDegree(output) += 1)

    val layers = mutable.ListBuffer[Chunk[NodeId]]()
    var zeroInputDegree = inputDegree
      .collect:
        case (id, 0) => id
      .toSet

    while zeroInputDegree.nonEmpty do
      layers += Chunk.from(zeroInputDegree)

      val nextZero = mutable.Set[NodeId]()

      for id <- zeroInputDegree do
        for output <- graph.getOutputs(id) do
          inputDegree(output) -= 1
          if (inputDegree(output) == 0) nextZero += output

        inputDegree.remove(id)

      zeroInputDegree = nextZero.toSet

    if inputDegree.nonEmpty then
      throw new AssertionError("Graph contains a cycle")

    Chunk.from(layers)