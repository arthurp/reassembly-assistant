package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath._

final class PortPlacement(val position: Vec2, val direction: Vec2) {
  assert(direction.length2 =~ 1)
  def matchingTransform(o: PortPlacement): Mat3 = {
    val trans = Mat3.translate(o.position, position)
    val rot = Mat3.rotate(position, -o.direction, direction)
    //println(s"Matching: $this, $o: rot\n$rot\ntrans\n$trans")
    trans * rot
  }

  def transform(trans: Mat3): PortPlacement = {
    PortPlacement(trans * position, trans *# direction)
  }

  override def toString = s"$position --> $direction"
}
object PortPlacement {
  def apply(position: Vec2, direction: Vec2): PortPlacement = new PortPlacement(position, direction.normalized)
  def apply(position: Vec2, direction: Real): PortPlacement = new PortPlacement(position, Vec2.fromAngle(direction))
  def apply(x: Real, y: Real, direction: Real): PortPlacement = new PortPlacement(Vec2(x, y), Vec2.fromAngle(direction))

  def unapply(p: PortPlacement): Option[(Vec2, Vec2)] = Some(p.position, p.direction)
}

case class Shape(vertices: Vector[Vec2], ports: Vector[PortPlacement]) {
  def lines = (vertices.view :+ vertices.head).sliding(2).map{ case Seq(a, b) => (a, b) }
  
  def flipped: Shape = transform(Mat3.scale(1, -1))

  def transform(trans: Mat3): Shape = {
    Shape(vertices.map(trans * _), ports.map(_.transform(trans)))
  }
  
  def centroid = vertices.reduce(_ + _) / vertices.size
  
  def overlaps(o: Shape): Boolean = Shape.overlapsOneDirection(this, o) && Shape.overlapsOneDirection(o, this)
}

object Shape {
  def overlapsOneDirection(s1full: Shape, s2: Shape): Boolean = {
    val s1 = s1full.transform(Mat3.scale(s1full.centroid, 1-Epsilons.COMPARE_EPSILON))
    //println(s"Checking $s1 lines against $s2")
    
    for ((a, b) <- s1.lines) {
      val v = (a - b).arbitraryPerpendicular
      //println(s"Checking line $a - $b: $v")
      // TODO: If performance is needed this could be optimized to compute max and min directly without building intermediate vectors.
      def projectPoints(s: Shape) = s.vertices map { p => 
        v * p
      }
      val projectedPoints1 = projectPoints(s1)
      val projectedPoints2 = projectPoints(s2)
      //println(projectedPoints1, projectedPoints2)
      val max1 = projectedPoints1.max
      val min1 = projectedPoints1.min
      val max2 = projectedPoints2.max
      val min2 = projectedPoints2.min
      //println(min1, max1, min2, max2)
      if (min1 > max2 || min2 > max1)
        return false
    }
    return true
  } 
}

object Shapes {
  val square = Shape(
    Vector(Vec2(1, 0), Vec2(1, 1), Vec2(0, 1), Vec2(0, 0)),
    Vector(
      PortPlacement(Vec2(0.5, 0), Vec2(0, -1)),
      PortPlacement(Vec2(0, 0.5), Vec2(-1, 0)),
      PortPlacement(Vec2(0.5, 1), Vec2(0, 1)),
      PortPlacement(Vec2(1, 0.5), Vec2(1, 0))))
  val rightTriangle = Shape(
    Vector(Vec2(1, 0), Vec2(0, 1), Vec2(0, 0)),
    Vector(
      PortPlacement(Vec2(0.5, 0), Vec2(0, -1)),
      PortPlacement(Vec2(0.5, 0.5), Vec2(1, 1)),
      PortPlacement(Vec2(0, 0.5), Vec2(-1, 0))))
  val longTriangle = Shape(
    Vector(Vec2(1, 0), Vec2(0, 2), Vec2(0, 0)),
    Vector(
      PortPlacement(Vec2(0.5, 0), Vec2(0, -1)),
      PortPlacement(Vec2(0, 0.5), Vec2(-1, 0)),
      PortPlacement(Vec2(0, 1.5), Vec2(-1, 0))))
}

