package org.singingwizard.swmath

import org.specs2._
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.runner.JUnitRunner
import org.singingwizard.swmath._
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.specs2.matcher.Matcher
import org.scalacheck.Gen

@RunWith(classOf[JUnitRunner])
class VecSpec extends mutable.Specification with ScalaCheck {
  implicit class extendVecApproxEqual(v: Vec2) {
    def ===~(w: Vec2) = {
      (v.x must beCloseTo(w.x, Epsilons.COMPARE_EPSILON)) and
        (v.y must beCloseTo(w.y, Epsilons.COMPARE_EPSILON))
    }
  }

  import VecSpec._

  "Vec2" >> {
    "identity" >> prop { (v: Vec2) ⇒
      Vec2(0, 0) + v ==== v
    }
    "Equality" >> prop { (v: Vec2) ⇒
      v ==== v
      v ===~ v
      v.## ==== v.##
      //v.approxHashcode ==== v.approxHashcode
    }
    /*
    "Approx hashcode" >> (prop { (v1: Vec2, voffset: Vec2) ⇒
      val v2 = v1 + Vec2(voffset.x, 0)
      v1 ===~ v2
      println(v1.x, v1.y)
      println(v2.x, v2.y)
      println((v1.x / (Epsilons.COMPARE_EPSILON*100)).floor.toInt)
      println((v2.x / (Epsilons.COMPARE_EPSILON*100)).floor.toInt)
      (v1.approxHashcode ==== v2.approxHashcode)
    }).setArbitrary2(arbVec2Epsilon)
    */
  }
}

object VecSpec {
  val genVec2Double = for {
    x ← Gen.choose(-1000.0, 1000.0)
    y ← Gen.choose(-1000.0, 1000.0)
  } yield Vec2(x, y)
  val genVec2Int = for {
    x ← Gen.choose(-1000, 1000)
    y ← Gen.choose(-1000, 1000)
  } yield Vec2(x, y)
  val genVec2 = Gen.oneOf(genVec2Double, genVec2Int)
  implicit lazy val arbVec2: Arbitrary[Vec2] = Arbitrary(genVec2)
  val genVec2Epsilon = for {
    x ← Gen.choose(-Epsilons.COMPARE_EPSILON, Epsilons.COMPARE_EPSILON)
    y ← Gen.choose(-Epsilons.COMPARE_EPSILON, Epsilons.COMPARE_EPSILON)
  } yield Vec2(x, y)
  val arbVec2Epsilon: Arbitrary[Vec2] = Arbitrary(genVec2Epsilon)
}