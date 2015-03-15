package org.singingwizard

import scala.collection.AbstractIterator

object Utils {
  def getTime() = System.nanoTime() / 1000000000.0

  def time[A](a: ⇒ A) = {
    val now = getTime()
    val result = a
    val secs = getTime() - now
    (result, secs)
  }
  def timePrint[A](name: String)(a: ⇒ A) = {
    val (r, secs) = time(a)
      println(s"$name: ${"%.4f".format(secs)} seconds")
    (r, secs)
  }
}

object SlidingPairsWrapping {
  implicit class SlidingPairsWithWrap[T](val iter: Iterator[T]) extends AnyVal {
    def slidingPairsWrapping = new AbstractIterator[(T, T)] {
      private val first = iter.next()
      private var prev: T = first
      private var emittedLast = false
      def hasNext = !emittedLast
      def next(): (T, T) = {
        if (iter.hasNext) {
          val p = prev
          val v = iter.next()
          prev = v
          (p, v)
        } else {
          emittedLast = true
          (prev, first)
        }
      }
    }
  }
  implicit class SlidingPairsWithWrapIterable[T](val it: Iterable[T]) extends AnyVal {
    def slidingPairsWrapping = it.iterator.slidingPairsWrapping
  }
  implicit class SlidingPairsWithWrapArray[T](val it: Array[T]) extends AnyVal {
    def slidingPairsWrapping = it.iterator.slidingPairsWrapping
  }
}