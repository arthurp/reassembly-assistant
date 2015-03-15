package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3

object TestTest extends App {
  val t1 = Mat3(
    -0.71, -0.71, 0.00,
    0.71, -0.71, -0.71)
  val t2 = Mat3(
    0.00, -1.00, -0.50,
    1.00, 0.00, -0.50)
  val p1 = PlacedPiece(t1, PieceKinds.wedge)
  val p2 = PlacedPiece(t2, PieceKinds.wedge)
  println(p1 overlaps p2)
  println(p1.boundingbox overlaps p2.boundingbox)
  println(p1.boundingbox, p2.boundingbox)
  val s1 = ShipSegment().add(p1).get
  println(s1.overlaps(p2))
}