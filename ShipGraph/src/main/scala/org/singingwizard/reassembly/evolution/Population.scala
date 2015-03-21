package org.singingwizard.reassembly.evolution

import org.singingwizard.swmath.Random
/*
case class Population(phynotypes: Seq[Phynotype]) {
  import Phynotype._, Population._

  /*lazy val phynotypes = individuals.par.map(g => {
    val p = Phynotype(g)
    p.score
    p
  }).seq.toSeq.sorted*/

  def best(n: Int) = phynotypes.take(n)

  def reproduce(mutations: Iterable[Mutation], crossovers: Set[Crossover], resultSize: Int = phynotypes.size): Population = {
    require(resultSize > 0)

    val breedingPool = best((BREEDING_POOL_SIZE * resultSize).toInt).toIndexedSeq
    val nNew = (REPLACEMENT_RATE * resultSize).toInt max (resultSize - phynotypes.size)
    val newIndividuals = (for (_ ← (0 until nNew).par) yield {
      val parents = (1 to NUMBER_OF_PARENTS).flatMap(_ ⇒ Random.uniformElement(breedingPool)).sortBy(-_.score)
      val crossover = Random.uniformElement(crossovers).get
      val mutation = (0 to N_MUTATIONS).foldLeft((x: Phynotype) ⇒ x: Genotype) { (m, _) ⇒
        { (p: Phynotype) ⇒
          m(Random.uniformElement(mutations).get(p))
        }
      }
      mutation(crossover(parents))
    })
    val newPhyns = phynotypes ++ buildPhynotypes(newIndividuals)
    Population(newPhyns.sorted.distinct.take(resultSize))
  }
}

object Population {
  val BREEDING_POOL_SIZE = 0.25
  val REPLACEMENT_RATE = 0.25
  val NUMBER_OF_PARENTS = 2
  val N_MUTATIONS = 5

  def apply(gs: Iterable[Genotype]): Population = Population(buildPhynotypes(gs.toSeq).sorted)

  private def buildPhynotypes(gs: scala.collection.GenSeq[Genotype]) = gs.par.map(g ⇒ {
    val p = Phynotype(g)
    p.score
    p
  }).seq

  import Phynotype._
  implicit val GenotypeOrdering: Ordering[Phynotype] = Ordering.by(-_.score)
}
*/