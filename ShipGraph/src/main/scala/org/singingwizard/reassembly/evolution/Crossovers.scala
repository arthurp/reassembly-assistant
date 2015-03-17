package org.singingwizard.reassembly.evolution

import org.singingwizard.reassembly.shipgraphs.PieceKind
import org.singingwizard.swmath.Random

case class Crossovers() {
  def fullOverride(parents: Seq[Genotype]): Genotype = {
    parents.head
  }
  
  val all: Set[Crossover] = Set(fullOverride)
}