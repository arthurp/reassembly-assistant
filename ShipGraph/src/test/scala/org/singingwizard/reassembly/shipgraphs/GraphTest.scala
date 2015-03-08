package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout

object GraphTest extends App {
  val (g1, n1) = Graph.empty.addNode(Shapes.square)
  val (g2, n2) = g1.addNode(Shapes.square)
  val (g3, n3) = g2.addNode(Shapes.rightTriangle)
  val g4 = (g3.
      connectPorts(n1.ports(0), g2.anchor.ports(0)).
      connectPorts(n2.ports(1), g2.anchor.ports(3)).
      connectPorts(n3.ports(1), n2.ports(0)))
  //val g3 = g1.connectPorts(n1.ports(0), true, g1.anchor.ports(0))
  val layout = GraphLayoutLens.layoutGraph(g4)
  println(layout.shapes.mkString("\n"))
  DrawLayout.show(layout)
  println(g4)
  println(GraphLayoutLens.graphLayout(g4, layout))
  /*assert(l.size == 2)
  assert(l.head.transform == Mat3.nil)*/
}