package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import org.singingwizard.reassembly.evolution.Phynotype
import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout
import org.singingwizard.reassembly.evolution.Population
import org.singingwizard.reassembly.evolution.Mutations
import org.singingwizard.reassembly.evolution.Crossovers

object TestTest extends App {
  val s = (1 until 10).foldLeft(Ship()) { (s, j) ⇒
    val p1 = PlacedPiece(Mat3.translate(j, j - 1), PieceKinds.squareWeak)
    val p2 = PlacedPiece(Mat3.translate(j, j), PieceKinds.squareWeak)
    val p3 = PlacedPiece(Mat3.translate(j - 1, j), PieceKinds.squareWeak)
    s.add(p1).get.add(p2).get.add(p3).get
  }
  
  import Phynotype._

  val kinds = Set(PieceKinds.squareStrong, PieceKinds.squareWeak, PieceKinds.largeTriangle, PieceKinds.wedge, PieceKinds.rightTriangle)

  var pop = Population((0 to 30).map(_ ⇒ GraphSpec.genShip.sample.get))
  DrawLayout.showMany { () ⇒
    pop = pop.reproduce(Mutations(kinds).all, Crossovers().all, 100)

    println(pop.phynotypes.map(_.score))
    pop.best(1).head.genes
  }
}