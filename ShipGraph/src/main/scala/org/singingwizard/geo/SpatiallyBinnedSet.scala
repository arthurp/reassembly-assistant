package org.singingwizard.geo

import org.singingwizard.swmath.Vec2
import org.singingwizard.swmath.Epsilons

class SpatiallyBinnedSet2[T] protected (range: Vec2, nBins: Int,
                                        protected val positionOf: T ⇒ Vec2,
                                        private[geo] val bins: Vector[Set[T]]) {
  assert(bins.size == nBins * nBins)

  @inline
  protected final def realmod(a: Int, b: Int) = (a % b + b) % b
  protected final def computeBin(p: Vec2) = {
    val x = p.x / range.x
    val y = p.y / range.y
    val i = realmod((x * nBins).floor.toInt, nBins)
    val j = realmod((y * nBins).floor.toInt, nBins)
    (i * nBins + j)
  }

  def subsetForPoint(p: Vec2) = bins(computeBin(p))

  def apply(p: Vec2): Set[T] = subsetForPoint(p)
  def apply(ps: TraversableOnce[Vec2]): Set[T] = {
    val res = Set.newBuilder[T]
    for (p ← ps) {
      res ++= subsetForPoint(p)
    }
    res.result()
  }
  
  def contains(e: T) = this(positionOf(e)) contains e

  def +(e: (Vec2, T)): SpatiallyBinnedSet2[T] = {
    val (p, elem) = e
    import SpatiallyBinnedSet2._
    val is = Set(computeBin(p + epsilonVec1), computeBin(p - epsilonVec1),
      computeBin(p + epsilonVec2), computeBin(p - epsilonVec2))
    new SpatiallyBinnedSet2(range, nBins, positionOf, is.foldLeft(bins)((bs, i) ⇒ bs.updated(i, bs(i) + elem)))
  }
  def +(e: T): SpatiallyBinnedSet2[T] = this + (positionOf(e), e)

  def ++(es: TraversableOnce[(Vec2, T)]) = {
    es.foldLeft(this)(_ + _)
  }
  def addValues(es: TraversableOnce[T]) = {
    es.foldLeft(this)(_ + _)
  }

  def values = bins.flatten.toSet
  def size = values.size
  def sizeAtMost = bins.map(_.size).sum
}

object SpatiallyBinnedSet2 {
  def apply[T](range: Vec2, size: Int, positionOf: T ⇒ Vec2) = {
    new SpatiallyBinnedSet2[T](range, size, positionOf, makeBins(size))
  }

  protected def makeBins[T](size: Int) = (0 until size * size).map(_ ⇒ Set[T]()).toVector

  private[geo] val epsilonVec1 = Vec2(Epsilons.COMPARE_EPSILON, Epsilons.COMPARE_EPSILON)
  private[geo] val epsilonVec2 = Vec2(Epsilons.COMPARE_EPSILON, -Epsilons.COMPARE_EPSILON)
}