package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath._

case class PlacedShape(id: Int, transform: Mat3, shape: Shape) {
  def overlaps(o: PlacedShape) = tshape overlaps o.tshape
  
  lazy val tshape = shape.transform(transform)
}

class Layout(val shapes: Seq[PlacedShape], val anchorID: Int) { 
  import Layout._
  
  def hasOverlaps: Boolean = { 
    for (s +: rest <- shapes.tails) {
      if (rest.exists(s overlaps _))
        return true
    }
    return false
  }
}

object Layout {
  def apply(shapes: Seq[PlacedShape], anchorID: Int) = new Layout(shapes, anchorID)
}