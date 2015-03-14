package org.singingwizard.geo

import org.singingwizard.swmath.Vec2
import org.singingwizard.swmath.Epsilons

final class SpatiallyBinnedSet2[T] private (range: Vec2, nBins: Int, private[geo] val bins: Vector[Set[T]]) {
  assert(bins.size == nBins*nBins)
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

  def apply(p: Vec2) = subsetForPoint(p)
  
  def +(e: (Vec2, T)) = {
    val (p, elem) = e
    import SpatiallyBinnedSet2._
    val is = Set(computeBin(p + epsilonVec1), computeBin(p - epsilonVec1), 
        computeBin(p + epsilonVec2), computeBin(p - epsilonVec2))
    new SpatiallyBinnedSet2(range, nBins, is.foldLeft(bins)((bs, i) => bs.updated(i, bs(i) + elem)))
  }
  
  def ++(es: Traversable[(Vec2, T)]) = {
    es.foldLeft(this)(_ + _)
  }
   
  def values = bins.flatten.toSet
  def size = values.size
}

object SpatiallyBinnedSet2 {
  def apply[T](range: Vec2, size: Int) = {
    new SpatiallyBinnedSet2[T](range, size, (0 until size*size).map(_ => Set[T]()).toVector)
  }
  
  val epsilonVec1 = Vec2(Epsilons.COMPARE_EPSILON, Epsilons.COMPARE_EPSILON)
  val epsilonVec2 = Vec2(Epsilons.COMPARE_EPSILON, -Epsilons.COMPARE_EPSILON)
}