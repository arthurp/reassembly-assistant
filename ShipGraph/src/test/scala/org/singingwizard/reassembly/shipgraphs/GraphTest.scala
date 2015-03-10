package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import scalaz.State
import scalax.collection.immutable.Graph

//import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout

object GraphTest extends App {
  import Ship._

  {
    val s = Ship()
    val s2 = s.attach(PieceKinds.squareWeak, 0, s.core.ports(0))
    
    println(s2)
  }
  {
    val e1 = PlacedPiece(Mat3.nil, PieceKinds.squareWeak)
    val e2 = PlacedPiece(Mat3.translate(1, 0), PieceKinds.squareWeak)
    val g = Graph[Port, Edge]() + e1 + e2 + (Port(0, e1) ~ Port(1, e2))
    println(g)
    println(g.nodes.size)
    println(g.edges.size)
    println(g.edges.map(_.edge).map(v => (v, v.getClass)))
    println(g.edges.map(_.edge).collect({ case p @ Piece(_) ⇒ p }).size)
    println(g.edges.map(_.edge).collect({ case c @ Connection(_, _) ⇒ c }).size)
  }
  /*DrawLayout.showMany { () ⇒
    val g = GraphSpec.genGraph.sample.get
    val gClean = CleaningAlgorithms.clean(g)
    val layout = GraphLayoutLens.layoutGraph(gClean)
    //println(layout.shapes.mkString("\n"))
    //println(layout.impossibleEdges.mkString("\n"))
    //println(gClean)
    //println(gClean.connectedComponents().map(_.mkString("\n")).mkString("\n===========\n"))
    layout
  }*/
}