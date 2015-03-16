package org.singingwizard.reassembly.evolution

import org.singingwizard.reassembly.shipgraphs.Ship._
import org.singingwizard.geo.AABB2

case class Phynotype(genes: Genotype) {
  import Phynotype._
  
  def pieceCountScore: Double = genes.pieceCount * PIECE_COUNT_FACTOR
  def massScore: Double = genes.pieces.foldLeft(0.0)(_ + _.kind.mass) * MASS_FACTOR
  def hpScore: Double = genes.pieces.foldLeft(0.0)(_ + _.kind.hitpoints) * HP_FACTOR
  def averagePathToCodeScore: Double = {
    val g = genes.graph
    val totalWeight = g.totalWeight
    // TODO: This should do SSSP.
    val pathLengths = for {
      p ← genes.pieces if p != genes.core
      port ← p.ports
    } yield {
      val invweightFunc: g.EdgeT ⇒ Long = {
        case Piece(p1) if p == p1 ⇒ 0
        case Piece(p1) ⇒ totalWeight - p1.kind.hitpoints
        case _ ⇒ 0
      }
      val weightFunc: g.EdgeT ⇒ Long = {
        case Piece(p1) if p == p1 ⇒ 0
        case Piece(p1) ⇒ p1.kind.hitpoints
        case _ ⇒ 0
      }
      g.outerEdgeTraverser(g.get(port)).shortestPathTo(g.get(genes.core.ports(0)), invweightFunc) match {
        case Some(path) ⇒ {
          //println(s"Found path $path with weight ${path.weight(weightFunc)}")
          path.weight(weightFunc)
        }
        case None ⇒ throw new Error(s"There should always be a path from every node to the core: $port")
      }
    }
    (pathLengths.sum.toDouble / pathLengths.size) * AVERAGE_PATH_TO_CORE_FACTOR
  }
  def boundingBoxScore: Double = {
    val boundingBoxes = genes.pieces.map(_.boundingbox)
    def bbSize(bb: AABB2) = {
      val v = bb.maximum - bb.minimum
      v.x.abs * v.y.abs
    }
    bbSize(boundingBoxes.reduce(_ + _)) * BOUNDING_BOX_FACTOR
  }
  
  def score: Double = {
    pieceCountScore + massScore + hpScore + averagePathToCodeScore + boundingBoxScore
  }
}

object Phynotype {
  val PIECE_COUNT_FACTOR = -1.0
  val MASS_FACTOR = -1.0
  val HP_FACTOR = 1.0
  val AVERAGE_PATH_TO_CORE_FACTOR = 1.0
  val BOUNDING_BOX_FACTOR = -1.0

  import scala.language.implicitConversions
  implicit def extendGenotypeWithPhynotype(g: Genotype): Phynotype = Phynotype(g)
}