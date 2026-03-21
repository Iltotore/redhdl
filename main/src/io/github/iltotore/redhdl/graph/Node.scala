package io.github.iltotore.redhdl.graph

import kyo.Chunk

/**
 * A single logic node in a [[Graph]].
 *
 * A node encapsulates the gate type it represents ([[NodeType]]) together with the
 * list of downstream [[NodeOutput]] edges it drives.  Nodes are immutable; mutations
 * return a new copy via [[withOutput]].
 *
 * @param tpe     The kind of logic gate (or I/O pin) this node represents.
 * @param outputs The outgoing edges from this node, each describing which input slot
 *                of a downstream node is driven by this node's output signal.
 */
case class Node(tpe: NodeType, outputs: Chunk[NodeOutput]):

  /**
   * Return a copy of this node with `output` appended to its output list.
   *
   * @param output The new [[NodeOutput]] edge to add.
   * @return A new [[Node]] with the same type but an extended output list.
   */
  def withOutput(output: NodeOutput): Node = this.copy(outputs = outputs :+ output)
