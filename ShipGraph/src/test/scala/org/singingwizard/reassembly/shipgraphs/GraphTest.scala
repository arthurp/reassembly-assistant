package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import scalaz.State
import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout
import org.singingwizard.swmath.Random
import org.singingwizard.reassembly.evolution.Phynotype

object GraphTest extends App {
  import Ship._

  var g = GraphSpec.genShip.sample.get
  DrawLayout.show {
    val s = GraphSpec.genSegment.sample.get
    //g = g.attach(s, Random.uniformElement(s.disconnectedPorts).get,
    //  Random.uniformElement(g.disconnectedPorts).get, allowPartial = true)
    println(Phynotype(g).minCuts)
    g
  }
}
