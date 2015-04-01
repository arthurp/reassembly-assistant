package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Mat3
import scalaz.State
import org.singingwizard.reassembly.shipgraphs.debug.DrawLayout
import org.singingwizard.swmath.Random
import org.singingwizard.reassembly.evolution.Phynotype

object GraphTest extends App {
  import Ship._

  var g = GraphSpec.genShip.sample.get
  DrawLayout.showMany(30) { () ⇒
    val s = GraphSpec.genSegment.sample.get
    g = g.attach(s, Random.uniformElement(s.disconnectedPorts).get,
      Random.uniformElement(g.disconnectedPorts).get, allowPartial = true)
    val p = Phynotype(g)
    val r = Ray.randomRay(10, 100, 0.5)
    println(s"Shooting $r")
    g.intersects(r) match {
      case Some(p) if p == g.core ⇒ {
        /*println(s"Destroyed: ${g.pieceCount}")
        g = GraphSpec.genShip.sample.get
        r = Ray.randomRay(10, 100, 0.5)
        */
      }
      case Some(p) ⇒ {
        println(s"Hit $p")
        g = g.remove(p)
      }
      case None ⇒ {
        println(s"Miss")
      }
    }
    g
  }
}
