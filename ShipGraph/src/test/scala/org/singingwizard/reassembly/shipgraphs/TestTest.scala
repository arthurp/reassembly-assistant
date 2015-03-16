package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import org.singingwizard.reassembly.evolution.Phynotype
import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout

object TestTest extends App {
  val s = (1 until 10).foldLeft(Ship()) { (s, j) ⇒
    val p1 = PlacedPiece(Mat3.translate(j, j - 1), PieceKinds.squareWeak)
    val p2 = PlacedPiece(Mat3.translate(j, j), PieceKinds.squareWeak)
    val p3 = PlacedPiece(Mat3.translate(j - 1, j), PieceKinds.squareWeak)
    s.add(p1).get.add(p2).get.add(p3).get
  }
  println(s.pieces.toSeq.map(_.kind.hitpoints))
  import Phynotype._
  println(s.hpScore)
  println(s.massScore)
  println(s.pieceCountScore)
  println(s.averagePathToCodeScore)
  println(s.boundingBoxScore)
  println(s.score)
  DrawLayout.show(s)
}