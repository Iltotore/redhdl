package io.github.iltotore.redhdl.graph

import kyo.Chunk

case class Node(tpe: NodeType, outputs: Chunk[NodeId]):

  def withOutput(id: NodeId): Node = this.copy(outputs = outputs :+ id)