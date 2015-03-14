package org.singingwizard.geo

import org.specs2._
import org.specs2.runner.JUnitRunner
import org.singingwizard.swmath._
import org.junit.runner.RunWith
import org.scalacheck._
import scala.math.Pi

@RunWith(classOf[JUnitRunner])
class AABBSpec extends mutable.Specification with ScalaCheck {
  import MatSpec._
  "One point AABB" >> prop { (v: Vec2) ⇒
    val b = AABB2(v)
    (b contains v) ==== true
    (b containsEpsilon v) ==== true
    (b overlaps b) ==== true
    (b.maximum) ==== v
    (b.minimum) ==== v
  }
  "Various sets" >> (prop { (values: (Set[Vec2], Set[Vec2])) ⇒
    val (vs, vs2) = values 
    (!vs.isEmpty && !vs2.isEmpty) ==> {
      val b = AABB2(vs)
      for (v ← vs) {
        (b contains v) ==== true
        (b containsEpsilon v) ==== true
        (b overlaps AABB2(v)) ==== true
      }

      val b2 = AABB2(vs2)
      (b overlaps b2) ==== true
      (b contains b2.maximum) ==== true
      (b contains b2.minimum) ==== true
      (b contains b2.maxXminY) ==== true
      (b contains b2.minXmaxY) ==== true

      (b overlaps b) ==== true
    }
  }).setArbitrary(Arbitrary {
    for (vs <- Gen.containerOf[Set, Vec2](genVec2); vs2 <- Gen.someOf(vs)) yield (vs, vs2.toSet)
  }).collect
}