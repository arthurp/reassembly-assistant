package org.singingwizard.geo

import org.specs2._
import org.specs2.runner.JUnitRunner
import org.singingwizard.swmath._
import org.junit.runner.RunWith
import org.scalacheck._
import scala.math.Pi

@RunWith(classOf[JUnitRunner])
class SpatiallyBinnedSetSpec extends mutable.Specification with ScalaCheck {
  import MatSpec._
/*  "empty SpatiallyBinnedSet2" >> {
    val s = SpatiallyBinnedSet2[Vec2](Vec2(10, 10), 4, x => x)
    s.values ==== Set()
    prop { (p: Vec2) =>
      s(p) ==== Set()
    }
  }
  "Should bin grid correctly" >> {
    val (a, b, c) = (Vec2(1, 1), Vec2(3, 3), Vec2(-1, 2))
    val s = (SpatiallyBinnedSet2[Vec2](Vec2(10, 10), 4, x => x) + 
        (a -> a) + 
        (b -> b) + 
        (c -> c))
    s.values ==== Set(a, b, c)
    prop { (p: Vec2) =>
      s(p).size must be_<=(1)
      (s(p) ==== Set(a)) or 
      (s(p) ==== Set(b)) or 
      (s(p) ==== Set(c)) or 
      (s(p) ==== Set())
    }
  }
  "Should bin boundries in multiple bins" >> {
    val (a, b, c, d, e) = (Vec2(0, 0), Vec2(1, 1), Vec2(-1, 2), Vec2(1, -2), Vec2(-1, -0.5))
    val s = (SpatiallyBinnedSet2[Vec2](Vec2(10, 10), 4, x => x) + 
        (a -> a) + 
        (b -> b) + 
        (c -> c) +
        (d -> d) +
        (e -> e)
        )
    s.values ==== Set(a, b, c, d, e)    
    s(b) ==== Set(a, b)
    s(c) ==== Set(a, c)
    s(d) ==== Set(a, d)
    s(e) ==== Set(a, e)
  }
  "Various sets" >> (prop { (vs: Set[Vec2]) => 
    val s = SpatiallyBinnedSet2[Vec2](Vec2(5, 5), 5, x => x) ++ vs.map(v => v -> v)
    for (v <- vs) {
      s(v) must contain(v)
      s(v + SpatiallyBinnedSet2.epsilonVec1/2) must contain(v)
      s(v - SpatiallyBinnedSet2.epsilonVec1/2) must contain(v)
      s(v + SpatiallyBinnedSet2.epsilonVec2/2) must contain(v)
      s(v - SpatiallyBinnedSet2.epsilonVec2/2) must contain(v)
    }
    s.size ==== vs.size
  })
  "Bounding box extraction" >> (prop { (vs: Set[Vec2]) => 
    val s = SpatiallyBinnedSet2[Vec2](Vec2(500, 500), 10, x => x) ++ vs.map(v => v -> v)
    for (vs <- vs.sliding(3)) {
      val bb = AABB2(vs)
      val subset = s(bb)
      for (v <- s.values if bb contains v) {
        subset must contain(v)
      }
    }
    s.size ==== vs.size
  })
  */
}