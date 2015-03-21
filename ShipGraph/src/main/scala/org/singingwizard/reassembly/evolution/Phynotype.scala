package org.singingwizard.reassembly.evolution

import org.singingwizard.reassembly.shipgraphs.Ship._
import org.singingwizard.geo.AABB2
import org.singingwizard.reassembly.shipgraphs.PlacedPiece
import org.singingwizard.swmath._
/*
case class Phynotype(genes: Genotype) {
  import Phynotype._
  val graph = genes.graph

  lazy val paths: scala.collection.Map[PlacedPiece, graph.Path] = {
    // TODO: This should do SSSP.
    // TODO: Right now this just searches from the 0th port of every piece. This will introduce bias.
    (for {
      p ← genes.pieces if p != genes.core
      port = p.ports(0)
    } yield {
      val invweightFunc: graph.EdgeT ⇒ Long = {
        case Piece(p1) if p == p1 ⇒ 0
        case Piece(p1) ⇒ 1
        case _ ⇒ 0
      }
      graph.outerEdgeTraverser(graph.get(port)).shortestPathTo(graph.get(genes.core.ports(0)), invweightFunc) match {
        case Some(path) ⇒ {
          //println(s"Found path $path with weight ${path.weight(weightFunc)}")
          p -> path
        }
        case None ⇒ throw new Error(s"There should always be a path from every node to the core: $port")
      }
    }).toMap
  }
  lazy val pathLengths = {
    paths.mapValues { path ⇒
      val p = path.startNode.value.piece
      val weightFunc: graph.EdgeT ⇒ Long = {
        case Piece(p1) if p == p1 ⇒ 0
        case Piece(p1) ⇒ p1.kind.hitpoints
        case _ ⇒ 0
      }
      path.weight(weightFunc)
    }
  }

  lazy val pieceCountScore: Double = genes.pieceCount * PIECE_COUNT_FACTOR
  lazy val massScore: Double = genes.pieces.foldLeft(0.0)(_ + _.kind.mass) * MASS_FACTOR
  lazy val hpScore: Double = genes.pieces.foldLeft(0.0)(_ + _.kind.hitpoints) * HP_FACTOR
  lazy val averagePathToCoreScore: Double = {
    if (pathLengths.isEmpty)
      0
    else
      (pathLengths.values.sum.toDouble / pathLengths.size) * AVERAGE_PATH_TO_CORE_FACTOR
  }
  lazy val maxPathToCoreScore: Double = {
    if (pathLengths.isEmpty)
      0
    else
      pathLengths.values.max * MAX_PATH_TO_CORE_FACTOR
  }
  lazy val boundingBoxScore: Double = {
    val boundingBoxes = genes.pieces.map(_.boundingbox)
    def bbSize(bb: AABB2) = {
      val v = bb.maximum - bb.minimum
      v.x.abs * v.y.abs
    }
    bbSize(boundingBoxes.reduce(_ + _)) * BOUNDING_BOX_FACTOR
  }

  lazy val angularInertiaScore: Double = {
    require(genes.core.tshape.centroid =~ Vec2(0, 0))
    genes.pieces.iterator.map({ p ⇒
      p.tshape.centroid.length ~^ 2 * p.kind.mass
    }).sum * ANGULAR_INERTIA_FACTOR
  }


  lazy val pieceConnectednessScore: Double = {
    // TODO: Make this a real useful thing.
    (genes.connectionCount.toDouble / genes.pieceCount) * PIECE_CONNECTEDNESS_SCORE
  }

  lazy val score: Double = {
    val v = pieceCountScore + massScore + hpScore +
      averagePathToCoreScore + boundingBoxScore +
      maxPathToCoreScore + angularInertiaScore + 
      pieceConnectednessScore
    def scoresStr = s"$pieceCountScore + $massScore + $hpScore + $averagePathToCoreScore + " +
                    s"$boundingBoxScore + $maxPathToCoreScore + $angularInertiaScore + " +
                    s"$pieceConnectednessScore"
    assert(!v.isInfinite, s"Got inf from: $scoresStr\n$genes")
    assert(!v.isNaN, s"Got NaN from: $scoresStr\n$genes")
    v
  }
}

object Phynotype {
  val PIECE_COUNT_FACTOR = 3.0
  val MASS_FACTOR = 0
  val HP_FACTOR = 0
  val AVERAGE_PATH_TO_CORE_FACTOR = -0.2
  val MAX_PATH_TO_CORE_FACTOR = -0.1
  val BOUNDING_BOX_FACTOR = 0
  val ANGULAR_INERTIA_FACTOR = -0.01
  val PIECE_CONNECTEDNESS_SCORE = 10.0

  import scala.language.implicitConversions
  implicit def extendGenotypeWithPhynotype(g: Genotype): Phynotype = Phynotype(g)
  implicit def extractGenotype(g: Phynotype): Genotype = g.genes
}
*/