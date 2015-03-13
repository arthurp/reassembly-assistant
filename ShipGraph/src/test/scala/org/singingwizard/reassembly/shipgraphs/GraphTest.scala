package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import scalaz.State
import scalax.collection.immutable.Graph
import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout
import org.singingwizard.swmath.Random

object GraphTest extends App {
  import Ship._
  
  if (false) {
    for(i <- 0 until 10000) {
      GraphSpec.genShip.sample
      println(i)
    }
  } else {
    var g = GraphSpec.genShip.sample.get
    DrawLayout.showMany { () ⇒
      val s = GraphSpec.genSegment.sample.get
      g = g.attach(s, Random.uniformElement(s.disconnectedPorts).get,
          Random.uniformElement(g.disconnectedPorts).get, allowPartial=true)
      g
    }
  }
}