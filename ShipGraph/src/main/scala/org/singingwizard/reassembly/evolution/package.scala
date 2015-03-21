package org.singingwizard.reassembly

import org.singingwizard.reassembly.shipgraphs.Ship

package object evolution {
  type Genotype = Ship
  
  /**
   * Mutate an individual.
   */
  //type Mutation = Phynotype => Genotype
  /**
   * A cross over function. This should always be called with a list sorted by fitness. 
   */
  //type Crossover = (Seq[Phynotype])  => Genotype
}