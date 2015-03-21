package org.singingwizard.reassembly.evolution

import org.singingwizard.reassembly.shipgraphs.PieceKind
import org.singingwizard.swmath.Random
import com.typesafe.scalalogging.StrictLogging
/*
case class Mutations(availablePieces: Set[PieceKind]) extends StrictLogging {
  val TRIES = 100
  
  def addPiece(ind: Phynotype): Genotype = {
    val piecekind = Random.uniformElement(availablePieces).get

    for (_ ← 0 until TRIES) {
      val attachPort = Random.uniformElement(ind.disconnectedPorts).get
      ind.attachGet(piecekind, Random.uniformInt(0, piecekind.ports.size), attachPort) match {
        case (child, Some(_)) ⇒ return child
        case _ ⇒ ()
      }
    }
    logger.info(s"Failed to attach piece to body: body=\n${ind.genes}\npiece=\n$piecekind")
    ind
  }
  def removePiece(ind: Phynotype): Genotype = {
    Random.uniformElement(ind.piecesWithoutCore) map { ind.remove(_) } getOrElse ind
  }

  def noop(ind: Phynotype): Genotype = ind

  val all: Iterable[Mutation] = Seq(addPiece, removePiece, removePiece, noop)
}
*/