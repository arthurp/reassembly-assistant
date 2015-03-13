package org.singingwizard.swmath

import scala.math._
import scala.reflect.ClassTag

abstract class Mat[M <: Mat[M]: ClassTag]() {
  val size: Int
  val values: Array[Real]
  protected def create(): M
  def copy(): M

  protected def assignValues(_vals: Array[Real]) {
    System.arraycopy(_vals, 0, values, 0, size * size)
  }

  protected def assignIdentity() {
    for (i ← 0 until size)
      this(i, i) = 1
  }

  @inline def apply(i: Int, j: Int) = values(i + j * size)
  @inline private[swmath] def update(i: Int, j: Int, v: Real) = values(i + j * size) = v

  def *(m: M) = {
    val r = create()
    for (
      i ← 0 until size;
      j ← 0 until size;
      k ← 0 until size
    ) {
      r(i, j) += this(i, k) * m(k, j)
    }
    r
  }

  def isIdentity: Boolean = {
    for (
      i ← 0 until size;
      j ← 0 until size
    ) {
      if (i == j) {
        if (!(this(i, j) =~ 1.0))
          return false
      } else if (!(this(i, j) =~ 0.0))
        return false
    }
    return true
  }

  def inverse: M = {
    val a = copy() // As a evolves from original mat into identity
    val b = create() // b evolves from identity into inverse(a)
    b.assignIdentity()

    for (K ← 0 until size) {
      //one in the pivot
      val factor = a(K, K)
      (0 until size).foreach(i ⇒ {
        a(K, i) /= factor
        b(K, i) /= factor
      })
      //zeroing the column
      for (L ← 0 until size if L != K) {
        val coefficient = a(L, K)
        (0 until size).foreach(i ⇒ {
          a(L, i) -= coefficient * a(K, i)
          b(L, i) -= coefficient * b(K, i)
        })
      }
    }

    assert((this * b).isIdentity)

    b
  }
  def transpose: M = {
    val b = create()

    for (
      i ← 0 until size;
      j ← 0 until size
    ) {
      b(i, j) = this(j, i)
    }

    b
  }

  override def toString = {
    val sb = new StringBuilder()
    for (i ← 0 until size) {
      sb.append((0 until size).map(this(_, i).toString(2)).mkString(","))
      if (i < size - 1) sb.append("\n")
    }
    sb.toString
  }

  override def equals(o: Any): Boolean = o match {
    case o: M if size == o.size ⇒
      for (
        i ← 0 until size;
        j ← 0 until size
      ) {
        if (this(i, j) != o(i, j)) {
          return false
        }
      }
      return true
    case _ ⇒ false
  }
  def =~(o: M): Boolean = {
    for (
      i ← 0 until size;
      j ← 0 until size
    ) {
      if (!(this(i, j) =~ o(i, j))) {
        return false
      }
    }
    return true
  }

}

final class Mat4 private (val values: Array[Real] = new Array[Real](4 * 4)) extends Mat[Mat4] with Serializable {
  val size = 4

  override def apply(i: Int, j: Int) = values(i + j * 4)
  override private[swmath] def update(i: Int, j: Int, v: Real) = values(i + j * 4) = v

  protected def create() = {
    val r = new Mat4()
    r
  }
  def copy() = {
    val r = new Mat4()
    r.assignValues(values)
    r
  }

  def upper33: Mat3 = {
    val r = new Mat3()
    for (
      i ← 0 until 3;
      j ← 0 until 3
    ) {
      r(i, j) = this(i, j)
    }
    r
  }

  override def *(m: Mat4) = {
    val r = new Mat4()
    for (
      i ← 0 until 4;
      j ← 0 until 4;
      k ← 0 until 4
    ) {
      r(i, j) += this(i, k) * m(k, j)
    }
    r
  }
  def *(p: Vec4) = {
    Vec4(
      values(0) * p(0) + values(1) * p(1) + values(2) * p(2) + values(3) * p(3),
      values(4) * p(0) + values(5) * p(1) + values(6) * p(2) + values(7) * p(3),
      values(8) * p(0) + values(9) * p(1) + values(10) * p(2) + values(11) * p(3),
      values(12) * p(0) + values(13) * p(1) + values(14) * p(2) + values(15) * p(3))

  }
  def *(p: Vec3) = {
    Vec3(values(0) * p(0) + values(1) * p(1) + values(2) * p(2) + values(3),
      values(4) * p(0) + values(5) * p(1) + values(6) * p(2) + values(7),
      values(8) * p(0) + values(9) * p(1) + values(10) * p(2) + values(11))
  }

  /*
  def *|(p: Vec3) = {
    (this *# p).normalized
  }
  
  def *#(p: Vec3) = {
    Vec3(values(0) * p(0) + values(1) * p(1) + values(2) * p(2),
      values(4) * p(0) + values(5) * p(1) + values(6) * p(2),
      values(8) * p(0) + values(9) * p(1) + values(10) * p(2))
  }
  */

  def normalmatrix = upper33.inverse.transpose
}

object Mat4 {
  def translate(x: Real, y: Real, z: Real) = {
    new Mat4(Array(
      1, 0, 0, x,
      0, 1, 0, y,
      0, 0, 1, z,
      0, 0, 0, 1))
  }
  def scale(x: Real, y: Real, z: Real) = {
    new Mat4(Array(
      x, 0, 0, 0,
      0, y, 0, 0,
      0, 0, z, 0,
      0, 0, 0, 1))
  }
  def rotate(x: Real, y: Real, z: Real, theta: Real) = {
    val cost = cos(theta)
    val sint = sin(theta)
    new Mat4(Array(
      cost + x ~^ 2 * (1 - cost), x * y * (1 - cost) - z * sint, x * z * (1 - cost) + y * sint, 0,
      y * x * (1 - cost) + z * sint, cost + y ~^ 2 * (1 - cost), y * z * (1 - cost) - x * sint, 0,
      z * x * (1 - cost) - y * sint, z * y * (1 - cost) + x * sint, cost + z ~^ 2 * (1 - cost), 0,
      0, 0, 0, 1))
  }

  def apply(d: Seq[Seq[Real]]) = {
    require(d.forall(_.size == 4))
    require(d.size == 4)

    new Mat4(d.flatten.toArray)
  }
}

final class Mat3 private[swmath] (val values: Array[Real] = new Array[Real](3 * 3)) extends Mat[Mat3] with Serializable {
  val size = 3

  protected def create() = {
    val r = new Mat3()
    r
  }
  def copy() = {
    val r = new Mat3()
    r.assignValues(values)
    r
  }

  def *|(p: Vec3) = {
    Vec3(values(0) * p(0) + values(1) * p(1) + values(2) * p(2),
      values(3) * p(0) + values(4) * p(1) + values(5) * p(2),
      values(6) * p(0) + values(7) * p(1) + values(8) * p(2)).normalized
  }
  def *(p: Vec3) = {
    Vec3(values(0) * p(0) + values(1) * p(1) + values(2) * p(2),
      values(3) * p(0) + values(4) * p(1) + values(5) * p(2),
      values(6) * p(0) + values(7) * p(1) + values(8) * p(2))
  }

  /**
   * Point transform
   */
  def *(p: Vec2) = {
    Vec2(values(0) * p(0) + values(1) * p(1) + values(2),
      values(3) * p(0) + values(4) * p(1) + values(5))
  }
  /**
   * Vector transform
   */
  def *#(p: Vec2) = {
    Vec2(values(0) * p(0) + values(1) * p(1),
      values(3) * p(0) + values(4) * p(1))
  }
}

object Mat3 {
  def translate(t: Vec2) = {
    new Mat3(Array(
      1, 0, t.x,
      0, 1, t.y,
      0, 0, 1))
  }
  def translate(x: Real, y: Real) = {
    new Mat3(Array(
      1, 0, x,
      0, 1, y,
      0, 0, 1))
  }
  def translate(from: Vec2, to: Vec2) = {
    val t = to - from
    new Mat3(Array(
      1, 0, t.x,
      0, 1, t.y,
      0, 0, 1))
  }

  def scale(x: Real, y: Real): Mat3 = {
    new Mat3(Array(
      x, 0, 0,
      0, y, 0,
      0, 0, 1))
  }

  def scale(pivot: Vec2, f: Real): Mat3 = {
    new Mat3(Array(
      f, 0, -pivot.x * (f - 1),
      0, f, -pivot.y * (f - 1),
      0, 0, 1))
  }

  def rotate(theta: Real): Mat3 = {
    val cost = cos(theta)
    val sint = sin(theta)
    new Mat3(Array(
      cost, -sint, 0,
      sint, cost, 0,
      0, 0, 1))
  }

  def rotate(from: Vec2, to: Vec2): Mat3 = {
    rotate(math.atan2(to.y, to.x) - math.atan2(from.y, from.x))
    /*val cost = from * to
    val sint = cosToSin(cost)
    new Mat3(Array(
      cost, -sint, 0,
      sint, cost, 0,
      0, 0, 1))*/
  }

  def rotate(pivot: Vec2, theta: Real): Mat3 = {
    val cost = cos(theta)
    val sint = sin(theta)
    new Mat3(Array(
      cost, -sint, pivot.x - cost * pivot.x - -sint * pivot.y,
      sint, cost, pivot.y - sint * pivot.x - cost * pivot.y,
      0, 0, 1))
  }

  def rotate(pivot: Vec2, from: Vec2, to: Vec2): Mat3 = {
    rotate(pivot, math.atan2(to.y, to.x) - math.atan2(from.y, from.x))
  }

  val nil = {
    val m = new Mat3()
    m.assignIdentity()
    m
  }
}
