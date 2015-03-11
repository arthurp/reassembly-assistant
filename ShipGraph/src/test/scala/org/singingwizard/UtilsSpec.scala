package org.singingwizard

import org.specs2._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import org.scalacheck._

@RunWith(classOf[JUnitRunner])
class UtilsSpec extends mutable.Specification with ScalaCheck {
  "slidingPairsWrapping" >> {
    import org.singingwizard.SlidingPairsWrapping._
    "on array" >> prop { (a: Array[Int]) ⇒
      !a.isEmpty ==> {
        a.slidingPairsWrapping.size ==== a.size and
          a.slidingPairsWrapping.toVector ==== (a.toSeq :+ a(0)).sliding(2).map(s ⇒ (s(0), s(1))).toVector
      }
    }
    "on iterable" >> prop { (a: Iterable[Char]) ⇒
      !a.isEmpty ==> {
        a.slidingPairsWrapping.size ==== a.size and
          a.slidingPairsWrapping.toVector ==== (a.toSeq :+ a.head).sliding(2).map(s ⇒ (s(0), s(1))).toVector
      }
    }
    "on iterator" >> prop { (a: Iterable[Option[Int]]) ⇒
      !a.isEmpty ==> {
        a.iterator.slidingPairsWrapping.size ==== a.size and
          a.iterator.slidingPairsWrapping.toVector ==== (a.toSeq :+ a.head).sliding(2).map(s ⇒ (s(0), s(1))).toVector
      }
    }
  }
}
