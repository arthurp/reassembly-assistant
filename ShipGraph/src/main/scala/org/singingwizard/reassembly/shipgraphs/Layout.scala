package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath._

case class PlacedShape(id: Graph.NodeID, transform: Mat3, shape: Shape) {
  def overlaps(o: PlacedShape) = tshape overlaps o.tshape

  lazy val tshape = shape.transform(transform)
}

case class Layout(
    val shapes: Seq[PlacedShape],
    val anchorID: Graph.NodeID,
    val impossibleEdges: Set[Graph.Edge],
    val missingEdges: Set[Graph.Edge]) {
  import Layout._

  def hasOverlaps: Boolean = !findOverlapping.isEmpty

  def hasImpossibleEdges = !impossibleEdges.isEmpty

  def findOverlapping(): Set[PlacedShape] = {
    (for (s +: rest ← (shapes.tail :+ shapes.head).tails if rest.exists(s overlaps _)) yield s).toSet
  }

  def removeImpossibleEdges(): Layout = copy(impossibleEdges = Set())
  def addMissingEdges(): Layout = copy(missingEdges = Set())

  def removeShape(s: PlacedShape): Layout = {
    assert(s.id != anchorID)
    copy(shapes = shapes.filterNot(_ == s))
  }
}

object Layout {
}