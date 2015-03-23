package org.singingwizard.reassembly.evolution

import org.singingwizard.reassembly.shipgraphs._
import org.singingwizard.reassembly.shipgraphs.Ship._
import org.singingwizard.geo.AABB2
import org.singingwizard.swmath._
import org.singingwizard.reassembly.shipgraphs.PlacedPiece
import scala.collection.mutable

case class Phynotype(genes: Genotype) {
  import Phynotype._
  val graph = genes.graph

  def minCut(sink: PlacedPiece, width: (PieceKind, PieceKind) => Int): Int = {
    // TODO: This algorithm needs to be changed to effectively work over the dual graph.
    //println(graph)
    //println(sink)
    var totalFlow = 0
    var flows = mutable.Map[(PlacedPiece, PlacedPiece), Int]().withDefaultValue(0)
    for (Connection(Port(_, p1), Port(_, p2)) ← graph.edges) {
      flows((p1, p2)) += width(p1.kind, p2.kind)
      flows((p2, p1)) += width(p1.kind, p2.kind)
    }
    //println(flows)
    for (
      Some(path) ← Stream.continually(graph.bfsPath(genes.core, sink, (a, b) ⇒ flows((b, a)) > 0)).
        takeWhile(_.isDefined)
    ) {
      val maxFlow = path.sliding(2).map({ p ⇒
        val Seq(p1, p2) = p
        flows((p1, p2))
      }).min
      //println(s"Residual path: $path = $maxFlow")
      //println(flows)

      for (Seq(u, v) ← path.sliding(2)) {
        flows((u, v)) -= maxFlow
        flows((v, u)) += maxFlow
      }
      totalFlow += maxFlow
      //println(totalFlow)
    }
    val r = totalFlow
    //println(s"Result: $r\n$flows")
    r
  }
  lazy val minCuts: scala.collection.Map[PlacedPiece, Int] = {
    //genes.piecesWithoutCore.map(p ⇒ p -> minCut(p, _.hitpoints + _.hitpoints)).toMap
    genes.piecesWithoutCore.map(p ⇒ p -> minCut(p, (_, _) => 1)).toMap
  }

  lazy val paths: scala.collection.Map[PlacedPiece, Iterable[PlacedPiece]] = {
    var tentatives = mutable.Map[PlacedPiece, (Int, Seq[PlacedPiece])]().withDefaultValue((Int.MaxValue, Seq()))
    var visited = mutable.Set[PlacedPiece]()
    val queue = new mutable.PriorityQueue[(PlacedPiece, Int, Seq[PlacedPiece])]()(Ordering.by(_._2))
    queue += ((genes.core, 0, Seq(genes.core)))
    tentatives += genes.core -> (0, Seq(genes.core))
    while (!queue.isEmpty) {
      val (curr, dist, path) = queue.dequeue()
      if (!visited.contains(curr)) {
        for (neigh ← graph.neighbors(curr) if !visited.contains(neigh)) {
          val (td, tp) = tentatives(neigh)
          val d = td min (dist + 1)
          val p = path :+ neigh
          tentatives(neigh) = (d, p)
          queue.enqueue((neigh, d, p))
        }
        visited += curr
      }
    }
    tentatives mapValues { v ⇒
      val (d, p) = v
      p
    }
  }
  lazy val pathLengths = {
    paths.mapValues { path ⇒
      /*
      val p = path.startNode.value.piece
      val weightFunc: graph.EdgeT ⇒ Long = {
        case Piece(p1) if p == p1 ⇒ 0
        case Piece(p1) ⇒ p1.kind.hitpoints
        case _ ⇒ 0
      }
      path.weight(weightFunc)
      */
      path.size
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

  lazy val minMinCutScore: Double = {
    if (minCuts.isEmpty)
      0
    else
      minCuts.values.min * MIN_MIN_CUT_FACTOR
  }
  lazy val averageMinCutScore: Double = {
    if (minCuts.isEmpty)
      0
    else
      (minCuts.values.sum.toDouble / minCuts.size) * AVERAGE_MIN_CUT_FACTOR
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
      pieceConnectednessScore + minMinCutScore + averageMinCutScore
    def scoresStr = s"$pieceCountScore + $massScore + $hpScore + $averagePathToCoreScore + " +
      s"$boundingBoxScore + $maxPathToCoreScore + $angularInertiaScore + " +
      s"$pieceConnectednessScore + $minMinCutScore + $averageMinCutScore"
    assert(!v.isInfinite, s"Got inf from: $scoresStr\n$genes")
    assert(!v.isNaN, s"Got NaN from: $scoresStr\n$genes")
    v
  }
}

object Phynotype {
  val PIECE_COUNT_FACTOR = 2.0
  val MASS_FACTOR = 0.0
  val HP_FACTOR = 0
  val AVERAGE_PATH_TO_CORE_FACTOR = 0
  val MAX_PATH_TO_CORE_FACTOR = 0
  val BOUNDING_BOX_FACTOR = 1.0
  val ANGULAR_INERTIA_FACTOR = -0.01
  val PIECE_CONNECTEDNESS_SCORE = 0
  val MIN_MIN_CUT_FACTOR = 1.0
  val AVERAGE_MIN_CUT_FACTOR = 1.0

  import scala.language.implicitConversions
  implicit def extendGenotypeWithPhynotype(g: Genotype): Phynotype = Phynotype(g)
  implicit def extractGenotype(g: Phynotype): Genotype = g.genes
}