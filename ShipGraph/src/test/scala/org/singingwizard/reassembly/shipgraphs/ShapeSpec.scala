package org.singingwizard.reassembly.shipgraphs

import org.specs2._
import org.specs2.runner.JUnitRunner
import org.singingwizard.swmath._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class ShapeSpec extends mutable.Specification {
  "Shape.overlaps" >> {
    "Overlaps self" >> {
      (Shapes.square overlaps Shapes.square) ==== true
    }
    "Does not overlap offset" >> {
      (Shapes.square overlaps Shapes.square.transform(Mat3.translate(2, 0))) ==== false
    }
    "Does not overlap touching" >> {
      (Shapes.square overlaps Shapes.square.transform(Mat3.translate(1, 0))) ==== false
      (Shapes.square overlaps Shapes.square.transform(Mat3.translate(0, 1))) ==== false
    }
    "Does overlap slightly overlapped" >> {
      (Shapes.square overlaps Shapes.square.transform(Mat3.translate(0.999, 0))) ==== true
      (Shapes.square overlaps Shapes.square.transform(Mat3.translate(0, 0.999))) ==== true
    }
    "Overlaps mostly overlapping" >> {
      (Shapes.square overlaps Shapes.rightTriangle) ==== true
    }
  }
  
  
  "regularPolygon" >> {
    import Shapes._
    "4" >> {
      val p = regularPolygon(4)
      p.vertices.size ==== 4
      p.lines.size ==== 4
      p.vertices.toSeq must contain ((_: Vec2)  =~ Vec2(-0.5, -0.5))
      p.vertices.toSeq must contain ((_: Vec2)  =~ Vec2(0.5, -0.5))
      p.vertices.toSeq must contain ((_: Vec2)  =~ Vec2(-0.5, 0.5))
      p.vertices.toSeq must contain ((_: Vec2)  =~ Vec2(0.5, 0.5))
    }
    "3" >> {
      val p = regularPolygon(3)
      p.vertices.size ==== 3
      p.lines.size ==== 3
      p.ports.size ==== 3
    }
    "8" >> {
      regularPolygon(8).lines.size ==== 8
    }
  }
}