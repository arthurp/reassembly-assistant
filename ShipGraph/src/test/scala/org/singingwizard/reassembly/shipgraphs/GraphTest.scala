package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import scalaz.State
import scalax.collection.immutable.Graph

import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout

object GraphTest extends App {
  import Ship._
  
  if (false) {
    for(i <- 0 until 10000) {
      GraphSpec.genShip.sample
      println(i)
    }
  } else {
    DrawLayout.showMany { () ⇒
      GraphSpec.genShip.sample.get
    }
  }
}