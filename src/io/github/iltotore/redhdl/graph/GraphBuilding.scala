package io.github.iltotore.redhdl.graph

import kyo.*

type GraphBuilding = Var[Graph]

object GraphBuilding:

  def run[A, S](body: A < (GraphBuilding & S)): (Graph, A) < S =
    Var.runTuple(Graph.empty)(body)
