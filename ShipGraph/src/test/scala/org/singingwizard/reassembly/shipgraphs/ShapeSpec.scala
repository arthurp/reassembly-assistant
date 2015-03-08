package org.singingwizard.reassembly.shipgraphs

import org.specs2._
import org.specs2.runner.JUnitRunner
import org.singingwizard.swmath._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class ShapeSpec extends mutable.Specification {
  sequential
  
  "Shape" >> {
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
      (Shapes.square overlaps Shapes.square.transform(Mat3.translate(0.99, 0))) ==== true
      (Shapes.square overlaps Shapes.square.transform(Mat3.translate(0, 0.99))) ==== true
    }
    "Overlaps mostly overlapping" >> {
      (Shapes.square overlaps Shapes.rightTriangle) ==== true
    }
  }
}