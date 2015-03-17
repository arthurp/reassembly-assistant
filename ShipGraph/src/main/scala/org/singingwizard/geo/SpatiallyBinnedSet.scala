package org.singingwizard.geo

import org.singingwizard.swmath.Vec2
import org.singingwizard.swmath.Epsilons
import scala.collection.IterableLike
import scala.collection.mutable.Builder

class SpatiallyBinnedSet2[T] protected (range: Vec2, nBins: Int,
                                        protected val positionOf: T ⇒ Vec2,
                                        private[geo] val bins: Vector[Set[T]])
    extends IterableLike[T, SpatiallyBinnedSet2[T]] with Iterable[T] {
  assert(bins.size == nBins * nBins)

  @inline
  protected final def realmod(a: Int, b: Int) = (a % b + b) % b
  protected final def computeBinGridPosition(p: Vec2) = {
    val x = p.x / range.x
    val y = p.y / range.y
    val i = realmod((x * nBins).floor.toInt, nBins)
    val j = realmod((y * nBins).floor.toInt, nBins)
    (i, j)
  }
  protected final def computeBinIndex(p: Vec2) = {
    val (i, j) = computeBinGridPosition(p)
    (i * nBins + j)
  }

  val binRange = range / nBins

  def subsetForPoint(p: Vec2) = bins(computeBinIndex(p))

  def apply(p: Vec2): Set[T] = subsetForPoint(p)
  def apply(bb: AABB2): Set[T] = {
    if (bb.width > range.x || bb.height > range.y) {
      values
    } else {
      val res = Set.newBuilder[T]
      for {
        x ← (bb.minimum.x to bb.maximum.x by binRange.x) :+ bb.maximum.x
        y ← (bb.minimum.y to bb.maximum.y by binRange.y) :+ bb.maximum.y
      } {
        res ++= subsetForPoint(Vec2(x, y))
      }
      res.result()
    }
  }
  /*def apply(ps: TraversableOnce[Vec2]): Set[T] = {
    val res = Set.newBuilder[T]
    for (p ← ps) {
      res ++= subsetForPoint(p)
    }
    res.result()
  }*/

  def contains(e: T) = this(positionOf(e)) contains e

  def +(e: (Vec2, T)): SpatiallyBinnedSet2[T] = {
    val (p, elem) = e
    import SpatiallyBinnedSet2._
    val is = Set(computeBinIndex(p + epsilonVec1), computeBinIndex(p - epsilonVec1),
      computeBinIndex(p + epsilonVec2), computeBinIndex(p - epsilonVec2))
    new SpatiallyBinnedSet2(range, nBins, positionOf, is.foldLeft(bins)((bs, i) ⇒ bs.updated(i, bs(i) + elem)))
  }  
  def +(e: T): SpatiallyBinnedSet2[T] = this + (positionOf(e), e)
  
  def -(e: (Vec2, T)): SpatiallyBinnedSet2[T] = {
    val (p, elem) = e
    import SpatiallyBinnedSet2._
    val is = Set(computeBinIndex(p + epsilonVec1), computeBinIndex(p - epsilonVec1),
      computeBinIndex(p + epsilonVec2), computeBinIndex(p - epsilonVec2))
    new SpatiallyBinnedSet2(range, nBins, positionOf, is.foldLeft(bins)((bs, i) ⇒ bs.updated(i, bs(i) - elem)))
  }
  def -(e: T): SpatiallyBinnedSet2[T] = this - (positionOf(e), e)

  def ++(es: TraversableOnce[(Vec2, T)]) = {
    es.foldLeft(this)(_ + _)
  }
  def addValues(es: TraversableOnce[T]) = {
    es.foldLeft(this)(_ + _)
  }
  def removeValues(es: TraversableOnce[T]) = {
    es.foldLeft(this)(_ - _)
  }

  def values = bins.flatten.toSet
  //def size = values.size
  def sizeAtMost = bins.map(_.size).sum

  def iterator: Iterator[T] = values.iterator
  //def seq: scala.collection.TraversableOnce[T] = this
  protected[this] override def newBuilder: Builder[T, SpatiallyBinnedSet2[T]] = new Builder[T, SpatiallyBinnedSet2[T]] {
    var result: SpatiallyBinnedSet2[T] = SpatiallyBinnedSet2[T](range, nBins, positionOf)  
    def +=(elem: T): this.type = {
      result += elem
      this
    }
    def clear(): Unit = {
      result = SpatiallyBinnedSet2[T](range, nBins, positionOf)
    }
  }
}

object SpatiallyBinnedSet2 {
  def apply[T](range: Vec2, size: Int, positionOf: T ⇒ Vec2) = {
    new SpatiallyBinnedSet2[T](range, size, positionOf, makeBins(size))
  }

  protected def makeBins[T](size: Int) = (0 until size * size).map(_ ⇒ Set[T]()).toVector

  private[geo] val epsilonVec1 = Vec2(Epsilons.COMPARE_EPSILON, Epsilons.COMPARE_EPSILON)
  private[geo] val epsilonVec2 = Vec2(Epsilons.COMPARE_EPSILON, -Epsilons.COMPARE_EPSILON)
}