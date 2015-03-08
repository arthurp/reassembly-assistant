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
  
  def hasOverlaps: Boolean = findOverlapping.isDefined
  
  def hasImpossibleEdges = !impossibleEdges.isEmpty
  
  def findOverlapping(): Option[PlacedShape] = {
    for (s +: rest <- (shapes.tail :+ shapes.head).tails) {
      if (rest.exists(s overlaps _))
        return Some(s)
    }
    return None
  }
  
  def removeOverlappingUntilNone(): Layout = {
    findOverlapping match {
      case Some(id) => removeShape(id).removeOverlappingUntilNone()
      case None => this
    }
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