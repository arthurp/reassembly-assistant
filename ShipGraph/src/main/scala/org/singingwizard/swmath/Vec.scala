package org.singingwizard.swmath

import scala.math._

sealed abstract class Vec[V <: Vec[V]] {
  protected def size: Int
  protected def values: IndexedSeq[Real]
  protected def create(v: IndexedSeq[Real]): V

  def apply(i: Int): Real

  @inline def *(o: V): Real = {
    var r: Real = 0
    for (i <- 0 until size) {
      r += this(i) * o(i)
    }
    r
  }

  @inline def +(o: V): V = {
    create(for (i <- 0 until size) yield this(i) + o(i))
  }
  @inline def *#(o: V): V = {
    create(for (i <- 0 until size) yield this(i) * o(i))
  }
  @inline def -(o: V): V = {
    create(for (i <- 0 until size) yield this(i) - o(i))
  }

  @inline def *(d: Real): V = {
    create(values.map(_ * d))
  }
  @inline def /(d: Real): V = {
    create(values.map(_ / d))
  }

  @inline def unary_- : V =  {
    create(values.map(-_))
  }
  def length2: Real = values.map(v => v * v).reduce(_ + _)

  def length: Real = sqrt(length2).asInstanceOf[Real]

  @inline def normalized: V = this / length

  def =~(o: V): Boolean = {
    for (i <- 0 until size if !(this(i) =~ o(i))) return false
    true
  }
  /*def approxHashcode: Int = {
    (0 until size).foldLeft(0)((acc, i) => (this(i) / (Epsilons.COMPARE_EPSILON*100)).floor.toInt ^ acc + 17)
  }*/
  
  override def toString = {
    val tags = if (length2 =~ 1) "n" else ""
    values.map(_.toString(4)).mkString("(", ",", s"${if (tags.size > 0) ";" else ""}$tags)")
  }
}

object Vec {
  /*implicit class RealVecOperations(val s: Real) extends AnyVal {
    def *[V <: Vec[V]](v: Vec[V]) = v * s
  }*/
}

final case class Vec2(x: Real, y: Real) extends Vec[Vec2] {
  @inline protected def size = 2
  @inline protected def values = Array(x, y)
  @inline def apply(i: Int) = i match {
    case 0 => x
    case 1 => y
  }
  @inline protected def create(v: IndexedSeq[Real]) = Vec2(v(0), v(1))

  override def *(o: Vec2): Real = x * o.x + y * o.y
  @inline override def +(o: Vec2): Vec2 = Vec2(x + o.x, y + o.y)
  @inline override def -(o: Vec2): Vec2 = Vec2(x - o.x, y - o.y)
  @inline override def *#(o: Vec2): Vec2 = Vec2(x * o.x, y * o.y)
  @inline override def unary_- : Vec2 = Vec2(-x, -y)

  @inline def clamped = Vec2(clamp(x), clamp(y))
  def arbitraryPerpendicular = Vec2(y, -x)
  def counterclockwisePerpendicular = Vec2(y, -x)
  
  @inline def sameHemisphere(o: Vec2) = {
    this * o >= 0
  }
  
  override def =~(o: Vec2): Boolean = x =~ o.x && y =~ o.y  
}
object Vec2 {
  def fromAngle(t: Real) = {
    val cost = cos(t)
    val sint = sin(t)
    Vec2(cost, sint)
  }
  
  def intersection(a1: Vec2, a2: Vec2, b1: Vec2, b2: Vec2): Vec2 = {
    val x = ( (a1.x*a2.y - a1.y*a2.x) * (b1.x-b2.x) - (a1.x-a2.x) * (b1.x*b2.y - b1.y*b2.x) ) / 
            ( (a1.x-a2.x)*(b1.y-b2.y) - (a1.y-a2.y)*(b1.x-b2.x) )
    val y = ( (a1.x*a2.y - a1.y*a2.x) * (b1.y-b2.y) - (a1.y-a2.y) * (b1.x*b2.y - b1.y*b2.x) ) /
            ( (a1.x-a2.x)*(b1.y-b2.y) - (a1.y-a2.y)*(b1.x-b2.x) )
    Vec2(x,y)
  }
}

final case class Vec3(x: Real, y: Real, z: Real) extends Vec[Vec3] {
  protected def size = 3
  @inline protected def values = Array(x, y, z)
  @inline def apply(i: Int) = i match {
    case 0 => x
    case 1 => y
    case 2 => z
  }
  @inline def update(i: Int, v: Real): Vec3 = i match {
    case 0 => Vec3(v, y, z)
    case 1 => Vec3(x, v, z)
    case 2 => Vec3(x, y, v)
  }
  @inline protected def create(v: IndexedSeq[Real]) = Vec3(v(0), v(1), v(2))

  override def length2: Real = x * x + y * y + z * z

  override def *(o: Vec3): Real = x * o.x + y * o.y + z * o.z
  @inline override def +(o: Vec3): Vec3 = Vec3(x + o.x, y + o.y, z + o.z)
  @inline override def -(o: Vec3): Vec3 = Vec3(x - o.x, y - o.y, z - o.z)
  @inline override def *#(o: Vec3): Vec3 = Vec3(x * o.x, y * o.y, z * o.z)

  @inline override def unary_- : Vec3 = Vec3(-x, -y, -z)

  @inline override def *(d: Real): Vec3 = Vec3(x * d, y * d, z * d)
  @inline override def /(d: Real): Vec3 = Vec3(x / d, y / d, z / d)

  @inline def ^(o: Vec3): Vec3 = {
    Vec3(y * o.z - z * o.y,
      z * o.x - x * o.z,
      x * o.y - y * o.x)
  }

  def arbitraryPerpendicular: Vec3 = {
    assert(length2 =~ 1)

    val c = if (z != 0) Vec3(1, 1, (x + y) / z) else Vec3(x, y, 1)
    (this ^ c.normalized).normalized
  }

  @inline def pointwiseMin(o: Vec3) = Vec3(min(x, o.x), min(y, o.y), min(z, o.z))
  @inline def pointwiseMax(o: Vec3) = Vec3(max(x, o.x), max(y, o.y), max(z, o.z))

  @inline def xy = Vec2(x, y)
  @inline def xz = Vec2(x, z)
  @inline def yz = Vec2(y, z)

  @inline override def normalized: Vec3 = this / length
  
  /**
   * Rotate this vector so Z points to n. x and y are arbitrary.
   */
  @inline def rotateZTo(n: Vec3) = {
    val i = n.arbitraryPerpendicular
    val j = i ^ n
    i*x + j*y + n*z
  }
  
  def sameHemisphere(o: Vec3) = {
    this * o >= 0
  }
}

final case class Vec4(x: Real, y: Real, z: Real, w: Real) extends Vec[Vec4] {
  @inline protected def size = 4
  @inline protected def values = IndexedSeq(x, y, z, w)
  @inline def apply(i: Int) = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case 3 => w
  }
  @inline protected def create(v: IndexedSeq[Real]) = Vec4(v(0), v(1), v(2), v(4))
}

final case class Color(r: Real, g: Real, b: Real) {
  /*assert(r >= 0)
  assert(g >= 0)
  assert(b >= 0)
  */

  def apply(i: Int) = i match {
    case 0 => r
    case 1 => g
    case 2 => b
  }

  @inline def +(o: Color): Color = {
    Color(r + o.r, g + o.g, b + o.b)
  }
  @inline def -(o: Color): Color = {
    Color(r - o.r, g - o.g, b - o.b)
  }
  @inline def *(o: Color): Color = {
    Color(r * o.r, g * o.g, b * o.b)
  }
  @inline def /(o: Color): Color = {
    Color(r / o.r, g / o.g, b / o.b)
  }
  @inline def max(o: Color): Color = {
    Color(r max o.r, g max o.g, b max o.b)
  }
  @inline def average(o: Color): Color = {
    Color((r + o.r)/2, (g + o.g)/2, (b + o.b)/2)
  }

  @inline def *(d: Real): Color = {
    Color(r * d, g * d, b * d)
  }
  @inline def /(d: Real): Color = {
    Color(r / d, g / d, b / d)
  }
  @inline def ~^(d: Real): Color = {
    Color(r ~^ d, g ~^ d, b ~^ d)
  }

  def recip = Color(if (r == 0) 0 else 1 / r, if (g == 0) 0 else 1 / g, if (b == 0) 0 else 1 / b)
  def sqrt = Color(math.sqrt(r), math.sqrt(g), math.sqrt(b))

  @inline def nonZero = r != 0 || b != 0 || g != 0

  @inline def clamped = Color(clamp(r), clamp(g), clamp(b))

  @inline def power = (r + g + b) / 3
  @inline def maximumPower = math.max(r, math.max(g, b))

  override def toString = s"C(${r.toString(3)}, ${g.toString(3)}, ${b.toString(3)})"
}

object Color {
  val one = Color(1, 1, 1)
  val zero = Color(0, 0, 0)

  val white = one
  val black = zero

  /*implicit class RealColorOperations(val s: Real) extends AnyVal {
    def *(c: Color) = c * s
    def /(c: Color) = Color(s / c.r, s / c.g, s / c.b) // s / c
  }*/
}
