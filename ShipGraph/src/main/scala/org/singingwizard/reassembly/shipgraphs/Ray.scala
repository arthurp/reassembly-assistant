package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath._

case class Ray(origin: Vec2, direction: Vec2) {
  assert(direction.normalized =~ direction)
}

object Ray {
  private def ring(inner: Real, outer: Real): Vec2 = {
    import Random._, math._
    val Vec2(u, v) = uniformSquare()
    val theta = 2 * Pi * u
    val r = (v * (outer - inner)) + inner
    Vec2(r * cos(theta), r * sin(theta))
  }

  def randomRay(minDist: Real, maxDist: Real, maxMisaim: Real) = {
    import Random._
    val target = uniformCircle() * maxMisaim
    val origin = ring(minDist, maxDist)
    Ray(origin, (target - origin).normalized)
  }
}