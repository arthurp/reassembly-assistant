package org.singingwizard.swmath

import scala.math._
import java.util.concurrent.ThreadLocalRandom

trait Sampler extends Any {
  def uniformInt(lower: Int, upper: Int): Int
  def uniformReal(): Real
  def uniformSquare(): Vec2
  def uniformCube(): Vec3
  def uniformSign(): Real

  def uniformCircle(): Vec2 = {
    val Vec2(u, v) = uniformSquare()
    val theta = 2 * Pi * u
    val r = sqrt(v)
    Vec2(r * cos(theta), r * sin(theta))
  }

  /**
   * Get a vector uniformly distributed on a sphere.
   */
  def uniformSphere(): (Vec3, Real) = {
    val Vec2(uu, vv) = uniformSquare()
    val theta = 2 * Pi * uu
    val u = vv * 2 - 1

    (Vec3(sqrt(1 - u * u) * cos(theta), sqrt(1 - u * u) * sin(theta), u), 1 / (4 * Pi))
  }

  /**
   * Get a vector uniformly distributed on the hemisphere with pole n.
   */
  def uniformHemisphere(n: Vec3): (Vec3, Real) = {
    val Vec2(uu, vv) = uniformSquare()
    val phi = 2 * Pi * uu
    val cos_theta = vv
    val sin_theta = cosToSin(cos_theta)

    (Vec3(cos(phi) * sin_theta, sin(phi) * sin_theta, cos_theta) rotateZTo n, 1 / (2 * Pi))
  }

  def cosineHemisphere(n: Vec3): (Vec3, Real) = {
    val Vec2(u, v) = uniformSquare()
    // u is sin^2 phi
    val sin_phi = sqrt(u)
    val theta = 2 * Pi * v

    val x = sin_phi * cos(theta)
    val y = sin_phi * sin(theta)

    val i = n.arbitraryPerpendicular
    val j = i ^ n
    val cos_phi = sqrt(max(0, 1 - u))

    val vec = i * x + j * y + n * cos_phi
    checkNormalized(vec)

    (vec, cos_phi / Pi)
  }

  def powerCosineHemisphere(n: Vec3, shine: Real): (Vec3, Real) = {
    val Vec2(u, v) = uniformSquare()
    val cos_theta = u ~^ (1 / (shine + 1))
    val sin_theta = cosToSin(cos_theta)
    val phi = 2 * Pi * v

    val x = sin_theta * cos(phi)
    val y = sin_theta * sin(phi)
    val z = cos_theta

    (Vec3(x, y, z) rotateZTo n, cos_theta ~^ shine * (shine + 1) / (2 * Pi))
  }
}

object Random extends Sampler {
  private val rng = new MersenneTwisterFast()

  private def nextInt(n: Int) = synchronized { rng.nextInt(n) }
  @inline
  private def nextDouble() = synchronized { rng.nextDouble() }
  private def nextBoolean() = synchronized { rng.nextBoolean() }

  def uniformInt(lower: Int, upper: Int): Int = {
    if (lower == upper)
      lower
    else {
      assert(lower < upper)
      nextInt(upper - lower) + lower
    }
  }

  def uniformElement[T](coll: Iterable[T]): Option[T] = {
    val i = uniformInt(0, coll.size)
    coll.view.drop(i).headOption
  }
  def uniformElement[T](coll: IndexedSeq[T]): Option[T] = {
    if (coll.isEmpty)
      None
    else
      Some(coll(uniformInt(0, coll.size)))
  }

  def uniformReal(): Real = nextDouble()
  @inline
  def uniformSquare(): Vec2 = synchronized {
    Vec2(rng.nextDouble(), rng.nextDouble())
  }
  @inline
  def uniformCube(): Vec3 = synchronized {
    Vec3(rng.nextDouble(), rng.nextDouble(), rng.nextDouble())
  }
  def uniformSign(): Real = if (nextBoolean()) 1 else -1
}

