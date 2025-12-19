package io.github.iltotore.redhdl.graph

import kyo.*
import io.github.iltotore.redhdl.ast.Identifier
import io.github.iltotore.redhdl.ir.SimplifiedComponent

type GraphBuilding = Var[Graph]

object GraphBuilding:

  def run[A, S](inputs: Chunk[Identifier])(body: A < (GraphBuilding & S)): (Graph, A) < S =
    Var.runTuple(Graph.fromInputs(inputs))(body)

  def buildGraph(component: SimplifiedComponent): Graph =
    println(pprint(component))
    run(component.inputs)(GraphBuilder.buildOutputsGraph(component.outputs)).eval._1