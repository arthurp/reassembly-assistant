package org.singingwizard.reassembly.evolution

import org.singingwizard.reassembly.shipgraphs.PieceKind
import org.singingwizard.swmath.Random

case class Mutations(availablePieces: Set[PieceKind]) {
  def addPiece(ind: Genotype): Genotype = {
    val piecekind = Random.uniformElement(availablePieces).get

    for (_ ← 0 until 10) {
      val attachPort = Random.uniformElement(ind.disconnectedPorts).get
      ind.attachGet(piecekind, Random.uniformInt(0, piecekind.ports.size), attachPort) match {
        case (child, Some(_)) ⇒ return child
        case _ ⇒ ()
      }
    }
    ind
  }
  def removePiece(ind: Genotype): Genotype = {
    val p = Random.uniformElement(ind.pieces).get
    ind.remove(p)
  }
  
  val all: Set[Mutation] = Set(addPiece, removePiece)
}