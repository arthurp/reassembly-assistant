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
  "Various sets" >> prop { (vs: Set[Vec2]) ⇒
    !vs.isEmpty ==> {
      val b = AABB2(vs)
      for (v ← vs) {
        (b contains v) ==== true
        (b containsEpsilon v) ==== true
        (b overlaps AABB2(v)) ==== true
      }
      
      // TODO: This is ugly and kinda unsound, but I don't know how else to do it at the moment.
      val vs2 = Gen.someOf(vs).sample.get.toSet + Gen.oneOf(vs.toSeq).sample.get
      
      (b overlaps AABB2(vs2)) ==== true
      
      (b overlaps b) ==== true
    }
  }
}