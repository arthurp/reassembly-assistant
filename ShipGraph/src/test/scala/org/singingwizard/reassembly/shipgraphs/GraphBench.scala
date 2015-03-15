package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import scalaz.State
import scalax.collection.immutable.Graph
import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout
import org.singingwizard.swmath.Random
import org.singingwizard.Utils.timePrint
import org.singingwizard.swmath.Vec2

object GraphBench extends App {
  import Ship._

  def printShipStats(lastShip: Ship) = {
    println(s"Port alignment checks: ${lastShip.portAlignmentChecks} (${lastShip.portCount})")
    println(s"Overlap checks: ${lastShip.overlapChecks} (${lastShip.pieceCount})")
  }

  for (_ ← 0 until 5) {
    timePrint("===================== Sample") {
      for (i ← 0 until 30) {
        GraphSpec.genShip.sample.get
      }
    }
    timePrint("===================== Line") {
      for (i ← 0 until 10) {
        var lastShip = null: Ship
        (1 until 100).foldLeft(Ship()) { (s, j) ⇒
          val t = Mat3.translate(j, 0)
          val p = PlacedPiece(t, PieceKinds.squareWeak)
          lastShip = s
          s.add(p).get
        }
        printShipStats(lastShip)
      }
    }
    timePrint("===================== Angled Blocks") {
      for (i ← 0 until 10) {
        var lastShip = null: Ship
        (1 until 34).foldLeft(Ship()) { (s, j) ⇒
          val p1 = PlacedPiece(Mat3.translate(j, j - 1), PieceKinds.squareWeak)
          val p2 = PlacedPiece(Mat3.translate(j, j), PieceKinds.squareWeak)
          val p3 = PlacedPiece(Mat3.translate(j - 1, j), PieceKinds.squareWeak)
          lastShip = s
          s.add(p1).get.add(p2).get.add(p3).get
        }
        printShipStats(lastShip)
      }
    }
    timePrint("===================== Angled Blocks Long Chain") {
      for (i ← 0 until 3) {
        var lastShip = null: Ship
        (1 until 100).foldLeft(Ship()) { (s, j) ⇒
          val p1 = PlacedPiece(Mat3.translate(j, j - 1), PieceKinds.squareWeak)
          val p2 = PlacedPiece(Mat3.translate(j, j), PieceKinds.squareWeak)
          val p3 = PlacedPiece(Mat3.translate(j - 1, j), PieceKinds.squareWeak)
          lastShip = s
          s.add(p1).get.add(p2).get.add(p3).get
        }
        printShipStats(lastShip)
      }
    }
  }
}