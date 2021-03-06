package org.singingwizard.geo

import org.singingwizard.swmath.Vec2
import org.singingwizard.swmath.Real
import org.singingwizard.swmath.Epsilons._

final class AABB2 private (
    private val minimum_x: Real, private val minimum_y: Real,
    private val maximum_x: Real, private val maximum_y: Real) {
  require(minimum_x <= maximum_x)
  require(minimum_y <= maximum_y)

  @inline
  def contains(p: Vec2) = {
    p.x >= minimum_x && p.x <= maximum_x &&
      p.y >= minimum_y && p.y <= maximum_y
  }

  @inline
  def containsEpsilon(p: Vec2) = {
    p.x >= minimum_x - COMPARE_EPSILON && p.x <= maximum_x + COMPARE_EPSILON &&
      p.y >= minimum_y - COMPARE_EPSILON && p.y <= maximum_y + COMPARE_EPSILON
  }

  def overlaps(o: AABB2) = {
    (this containsEpsilon o.minimum) ||
      (this containsEpsilon o.maximum) ||
      (this containsEpsilon o.minXmaxY) ||
      (this containsEpsilon o.maxXminY) ||
      (o containsEpsilon minimum) ||
      (o containsEpsilon maximum) ||
      (o containsEpsilon minXmaxY) ||
      (o containsEpsilon maxXminY)
  }

  def width = maximum_x - minimum_x
  def height = maximum_y - minimum_y

  def minimum = Vec2(minimum_x, minimum_y)
  def maximum = Vec2(maximum_x, maximum_y)
  def minXmaxY = Vec2(minimum_x, maximum_y)
  def maxXminY = Vec2(maximum_x, minimum_y)

  def +(v: Vec2) = {
    new AABB2(minimum_x min v.x, minimum_y min v.y,
      maximum_x max v.x, maximum_y max v.y)
  }
  def +(o: AABB2) = {
    new AABB2(minimum_x min o.minimum_x, minimum_y min o.minimum_y,
      maximum_x max o.maximum_x, maximum_y max o.maximum_y)
  }
  
  override def toString = {
    s"AABB($minimum, $maximum)"
  }
}

object AABB2 {
  def apply(v: Vec2): AABB2 = {
    new AABB2(v.x, v.y, v.x, v.y)
  }
  def apply(vs: Traversable[Vec2]): AABB2 = {
    val v = vs.head
    vs.tail.foldLeft(AABB2(v))(_ + _)
  }
}