package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath._
import scalaz.syntax.std.ToVectorOps

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
  val square = regularPolygon(4)
  val equilateralTriangle = regularPolygonTwoPortsPerSide(3)
  val smallRectangle = Shape(
    Vector(Vec2(1, 0), Vec2(1, 0.5), Vec2(0, 0.5), Vec2(0, 0)),
    Vector(
      PortPlacement(Vec2(0.5, 0), Vec2(0, -1)),
      PortPlacement(Vec2(0, 0.25), Vec2(-1, 0)),
      PortPlacement(Vec2(0.5, 0.5), Vec2(0, 1)),
      PortPlacement(Vec2(1, 0.25), Vec2(1, 0))))
  val rightTriangle = Shape(
    Vector(Vec2(1, 0), Vec2(0, 1), Vec2(0, 0)),
    Vector(
      PortPlacement(Vec2(0.5, 0), Vec2(0, -1)),
      PortPlacement(Vec2(0.5, 0.5), Vec2(1, 1)),
      PortPlacement(Vec2(0, 0.5), Vec2(-1, 0))))
      
  val longEdgeCenter = (Vec2(1, 0) + Vec2(0, 2)) / 2
  val longEdgeVec = (Vec2(1, 0) - Vec2(0, 2)).normalized / 2
  val longEdgeNorm = Vec2(2, 1)
  val longTriangle = Shape(
    Vector(Vec2(1, 0), Vec2(0, 2), Vec2(0, 0)),
    Vector(
      PortPlacement(Vec2(0.5, 0), Vec2(0, -1)),
      PortPlacement(Vec2(0, 0.5), Vec2(-1, 0)),
      PortPlacement(Vec2(0, 1.5), Vec2(-1, 0)),
      PortPlacement(longEdgeCenter + longEdgeVec, longEdgeNorm),
      PortPlacement(longEdgeCenter - longEdgeVec, longEdgeNorm)
      ))

  def regularPolygon(n: Int): Shape = {
    val centers = (0.0 to 2*math.Pi by (2*math.Pi / n)).take(n).map(t => Vec2.fromAngle(t) / 2)
    val corners = (for (Seq(c1, c2) <- (centers :+ centers.head).sliding(2)) yield {
      val p1 = c1.counterclockwisePerpendicular
      val p2 = c2.counterclockwisePerpendicular
      Vec2.intersection(c1, c1+p1, c2, c2+p2)
    }).toVector
    val ports = for (c <- centers) yield PortPlacement(c, c) 
    
    Shape(corners, ports.toVector)
  }
  def regularPolygonTwoPortsPerSide(n: Int): Shape = {
    val s = regularPolygon(n)
    val lines = s.lines.toIterable
    val ports = for (((v1, v2), p) <- lines zip (s.ports.tail :+ s.ports.head)) yield {
      val v = (v1 - v2).normalized / 2
      Seq(PortPlacement(p.position + v, p.direction), PortPlacement(p.position - v, p.direction))
    }
    
    s.copy(ports = ports.flatten.toVector)
  }
      
  val octogon = regularPolygon(8)
}

