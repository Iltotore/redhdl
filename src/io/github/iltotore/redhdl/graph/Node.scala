package io.github.iltotore.redhdl.graph

import kyo.Chunk

case class Node(tpe: NodeType, outputs: Chunk[NodeOutput]):

  def withOutput(output: NodeOutput): Node = this.copy(outputs = outputs :+ output)
