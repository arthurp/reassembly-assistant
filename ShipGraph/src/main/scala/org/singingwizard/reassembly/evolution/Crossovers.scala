package org.singingwizard.reassembly.evolution

import org.singingwizard.reassembly.shipgraphs.PieceKind
import org.singingwizard.swmath.Random
import org.singingwizard.reassembly.shipgraphs.Ship._
import org.singingwizard.reassembly.shipgraphs.ShipSegment
import com.typesafe.scalalogging.StrictLogging
/*
case class Crossovers() extends StrictLogging {
  val TRIES = 100

  def fullOverride(parents: Seq[Phynotype]): Genotype = {
    Random.uniformElement(parents).get
  }

  def splicePart(parents: Seq[Phynotype]): Genotype = {
    val body = Random.uniformElement(parents).get
    val partDonor = Random.uniformElement(parents).get
    val partSeparator = Random.uniformElement(partDonor.piecesWithoutCore).getOrElse {
      return body
    }

    val pieces = partDonor.piecesWithoutCore.filter({ p ⇒
      val path = partDonor.paths(p)
      path.edges.exists({
        case Piece(p) ⇒ p == partSeparator
        case _ ⇒ false
      })
    }) + partSeparator - partDonor.core

    assert(pieces contains partSeparator)

    val traverser = partDonor.graph.outerEdgeTraverser(partDonor.graph.get(pieces.head.ports(0)),
      subgraphEdges = {
        case Piece(p) ⇒ pieces contains p
        case Connection(_, _) ⇒ true
        case _ ⇒ false
      }).collect({
        case Piece(p) ⇒ p
      }).toSeq.distinct

    val partOpt = traverser.foldLeft(Some(ShipSegment()): Option[ShipSegment])((seg, p) ⇒ seg.flatMap(_.add(p)))
    assert(partOpt.isDefined, s"Failed to make segment: $traverser")
    val part = partOpt.get

    //logger.debug(s"Attaching part to body: body=\n$body\npart=\n$part\ndonor=\n$partDonor")
    for (_ ← 0 until TRIES) {
      val attachPortBody = Random.uniformElement(body.disconnectedPorts).get
      val attachPortPart = Random.uniformElement(part.disconnectedPorts).get
      body.attachGet(part, attachPortPart, attachPortBody, allowPartial = true) match {
        case (child, leftOut) if leftOut.size < part.pieceCount ⇒ return child
        case _ ⇒ {
          //logger.trace(s"Failed to attach attempt: bodyPort=$attachPortBody partPort=$attachPortPart")
          ()
        }
      }
    }
    logger.info(s"Failed to attach part to body: body=\n$body\npart=\n$part")
    fullOverride(parents)
  }

  val all: Set[Crossover] = Set(fullOverride, splicePart)
}
*/