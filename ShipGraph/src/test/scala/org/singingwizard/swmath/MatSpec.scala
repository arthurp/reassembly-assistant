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
class MatSpec extends mutable.Specification with ScalaCheck {
  implicit def extendVecApproxEqual(v: Vec2) = new {
    def ===~(w: Vec2) = {
      (v.x must beCloseTo(w.x, Epsilons.COMPARE_EPSILON)) and
        (v.y must beCloseTo(w.y, Epsilons.COMPARE_EPSILON))
    }
  }

  import MatSpec._

  "Mat3" >> {
    "Mat3 identity" >> prop { (v: Vec2) ⇒
      Mat3.nil * v ==== v
    }
    "Mat3 rotate to" >> prop { (d: Real) ⇒
      d.abs <= 1000 ==> (Mat3.rotate(Vec2(1, 0), Vec2(0, 1)) * Vec2(d, 0) ===~ Vec2(0, d))
    }
    "Mat3 rotate to" >> {
      val d = 1
      Mat3.rotate(Vec2(1, 0), Vec2(0, 1)) * Vec2(d, 0) ===~ Vec2(0, d)
    }
    "Mat3 rotate to with pivot" >> {
      val m = Mat3.rotate(Vec2(10, 0), Vec2(1, 0), Vec2(0, 1))
      m * Vec2(10, 0) ==== Vec2(10, 0)
      Mat3.rotate(Vec2(1, 0), Vec2(1, 0), Vec2(0, 1)) * Vec2(2, 0) ==== Vec2(1, 1)
    }
  }
}

object MatSpec {
  val genVec2 = for {
    x ← Gen.choose(-1000.0, 1000.0)
    y ← Gen.choose(-1000.0, 1000.0)
  } yield Vec2(x, y)
  implicit lazy val arbVec2: Arbitrary[Vec2] = Arbitrary(genVec2)
}