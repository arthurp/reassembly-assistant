package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout

import scalaz.State

object GraphTest extends App {
  import Graph._
  /*val build: State[Graph, Unit] = for {
    n1 ← addNode(Shapes.square)
    n2 ← addNode(Shapes.square)
    n3 ← addNode(Shapes.square)
    n4 ← addNode(Shapes.longTriangle)
    anchor ← getAnchor
    _ ← connectPorts(n1.ports(0), anchor.ports(0))
    _ ← connectPorts(n2.ports(1), anchor.ports(3))
    _ ← connectPorts(n3.ports(1), n2.ports(0))
    _ ← connectPorts(n1.ports(3), n4.ports(2))
  } yield ()

  val g = build exec Graph.empty
  */
  DrawLayout.showMany { () ⇒
    val g = GraphSpec.genGraph.sample.get
    val gClean = CleaningAlgorithms.clean(g)
    val layout = GraphLayoutLens.layoutGraph(gClean)
    //println(layout.shapes.mkString("\n"))
    //println(layout.impossibleEdges.mkString("\n"))
    //println(gClean)
    //println(gClean.connectedComponents().map(_.mkString("\n")).mkString("\n===========\n"))
    layout
  }
  /*assert(l.size == 2)
  assert(l.head.transform == Mat3.nil)*/
}