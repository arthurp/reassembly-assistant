package org.singingwizard.reassembly.shipgraphs

import org.specs2._
import org.specs2.runner.JUnitRunner
import org.singingwizard.swmath._
import org.junit.runner.RunWith
import org.scalacheck._
import scalaz._
import scalax.collection.GraphEdge.EdgeException
import scala.math.Pi

@RunWith(classOf[JUnitRunner])
class GraphSpec extends mutable.Specification with ScalaCheck {
  import Ship._
  import scalax.collection.immutable.Graph

  "A graph of Ports" >> {
    "Single piece graph" >> {
      val e = PlacedPiece(Mat3.nil, PieceKinds.squareWeak)
      val g = Graph[Port, Edge]() + e
      g.nodes.size ==== 4
      g.edges.size ==== 1
      g.get(e).nodes.toSet ==== g.nodes.toSet
    }
    "Single octagon graph" >> {
      val e = PlacedPiece(Mat3.nil, PieceKinds.core)
      val g = Graph[Port, Edge]() + e
      g.nodes.size ==== 8
      g.edges.size ==== 1
      g.get(e).nodes.toSet ==== g.nodes.toSet
    }
    "Self loop" >> {
      val e = PlacedPiece(Mat3.nil, PieceKinds.squareWeak)
      val g = Graph[Port, Edge]() + e
      g + (Port(0, e) ~ Port(1, e)) must throwA[EdgeException]
    }
    "Two piece graph" >> {
      val e1 = PlacedPiece(Mat3.nil, PieceKinds.squareWeak)
      val e2 = PlacedPiece(Mat3.translate(1, 0), PieceKinds.squareWeak)
      val g = Graph[Port, Edge]() + e1 + e2 + (Port(0, e1) ~ Port(1, e2))
      g.nodes.size ==== 8
      g.edges.size ==== 3
      g.edges.map(_.edge).count({ case Piece(_) ⇒ true; case _ ⇒ false }) ==== 2
      g.edges.map(_.edge).count({ case Connection(_, _) ⇒ true; case _ ⇒ false }) ==== 1
    }
  }

  import GraphSpec._
  "A ship graph" >> {
    "Empty has one node" >> {
      Ship().pieceCount must_== 1
      Ship().pieces must have size (1)
    }
    "Adding one node should have 2 nodes" >> {
      val s1 = Ship()
      val s2 = s1.attach(PieceKinds.squareWeak, 0, s1.core.ports(0))
      s2.pieceCount must_== 2
      s2.pieces must have size (2)
    }
    "Removing an attaching node leaves only nodes on the core side" >> {
      val s = (1 until 3).foldLeft(Ship()) { (s, j) ⇒
        val t = Mat3.translate(j, 0)
        val p = PlacedPiece(t, PieceKinds.squareWeak)
        s.add(p).get
      }
      s.pieceCount must_== 3
      val s1 = s.remove(s.pieces.find(_.tshape.centroid =~ Vec2(1, 0)).get)
      s1.pieceCount must_== 1
      s1.validate() must_== None
    }
    "should fail to add long peice overlapping with other piece" >> {
      val s1 = Ship()
      s1.overlaps(PlacedPiece(Mat3.scale(8, 8), PieceKinds.squareWeak)) must be_==(true)
    }
    "should fail to add two slightly overlapping peices" >> {
      val t1 = Mat3(
        -0.71, -0.71, 0.00,
        0.71, -0.71, -0.71)
      val t2 = Mat3(
        0.00, -1.00, -0.50,
        1.00, 0.00, -0.50)
      val p1 = PlacedPiece(t1, PieceKinds.wedge)
      val p2 = PlacedPiece(t2, PieceKinds.wedge)
      (p1 overlaps p2) ==== true
      (p1.boundingbox overlaps p2.boundingbox) ==== true
      val s1 = ShipSegment().add(p1).get
      s1.overlaps(p2) must be_==(true)
    }
    "Nodes should be connected both ways" >> {
      val s1 = Ship()
      val (s2, Some(n)) = s1.attachGet(PieceKinds.squareWeak, 0, s1.core.ports(0))
      s2.pieceCount must_== 2
      s2.pieces must have size (2)
      s2.connectionCount must_== 1
      s2.connections must have size (1)
      val c = s2.connections.head
      c._1.id ==== 0
      c._2.id ==== 0
      c.isAt(n.ports(0)) ==== true
      c.isAt(s2.core.ports(0)) ==== true
    }
    "All spec generated ships are valid (incl after edits)" >> (prop { (s: Ship, pieceKind: PieceKind, portID: Int) ⇒
      s.validate ==== None
      val ports = s.ports.toIndexedSeq
      s.attach(pieceKind, 0, ports(portID % ports.size)).validate ==== None
      s.remove(ports(portID % ports.size).piece).validate ==== None
    }).setArbitrary3(Arbitrary(Gen.posNum[Int]))
  }

  "A ship segment graph" >> {
    "Empty has no nodes or edges" >> {
      ShipSegment().pieceCount must_== 0
      ShipSegment().pieces must have size (0)
      ShipSegment().connections must have size (0)
    }
    "We can splice one onto another full succeed" >> {
      val s1 = Ship()
      val (s2, Some(p1)) = s1.attachGet(PieceKinds.squareWeak, 0, s1.core.ports(0))
      val p2 = PlacedPiece(Mat3.nil, PieceKinds.squareStrong)
      val Some(seg) = ShipSegment().add(p2)
      val (s3, ps) = s2.attachGet(seg, seg.disconnectedPorts.head, s2.get(p1).ports(1),
        allowPartial = false)
      ps.size ==== 0
      s3.pieceCount ==== 3
    }
    "We can splice one onto another partial" >> {
      val s1 = Ship()
      val (s2, Some(p1)) = s1.attachGet(PieceKinds.squareWeak, 0, s1.core.ports(0))
      val p2 = PlacedPiece(Mat3.nil, PieceKinds.squareStrong)
      val p3 = PlacedPiece(Mat3.translate(1, 0), PieceKinds.squareStrong)
      val p4 = PlacedPiece(Mat3.translate(0, 1), PieceKinds.squareStrong)
      val Some(seg) = ShipSegment().add(p2).flatMap(_.add(p3).flatMap(_.add(p4)))
      val (s3, ps) = s2.attachGet(seg, seg.get(p4).ports(0), s2.get(p1).ports(3),
        allowPartial = true)
      ps.size ==== 1
      s3.pieceCount ==== 4
    }
    "We can splice one onto another (allow partial)" >> prop { (s1: ShipSegment, s2: ShipSegment) ⇒
      val (s, ps) = s1.attachGet(s2, Random.uniformElement(s2.disconnectedPorts).get,
        Random.uniformElement(s1.disconnectedPorts).get, allowPartial = true)
      if (s ne s1) {
        ps.size must be_!=(s2.pieceCount)
        s.pieceCount ==== s1.pieceCount + s2.pieceCount - ps.size
      } else {
        ps.size ==== s2.pieceCount
      }
      s.validate ==== None
    }
    "We can splice one onto another (disallow partial)" >> prop { (s1: ShipSegment, s2: ShipSegment) ⇒
      val p2 = Random.uniformElement(s2.disconnectedPorts).get
      val p1 = Random.uniformElement(s1.disconnectedPorts).get
      val (s, ps) = s1.attachGet(s2, p2, p1, allowPartial = false)
      if (s ne s1) {
        ps.size ==== 0
        s.pieceCount ==== s1.pieceCount + s2.pieceCount
      } else {
        ps.size ==== s2.pieceCount
      }
      s.validate ==== None
    }
    "All spec generated ship segments are valid" >> prop { (s: ShipSegment) ⇒
      s.validate ==== None
    }
  }
}

object GraphSpec {
  import org.singingwizard.swmath.MatSpec._

  val genShape = Gen.oneOf(
    Shapes.square,
    Shapes.rightTriangle,
    Shapes.longTriangle,
    Shapes.longTriangle.flipped,
    Shapes.octogon,
    Shapes.smallRectangle,
    Shapes.largeEquilateralTriangle,
    Shapes.smallEquilateralTriangle,
    Shapes.regularPolygon(6)
  )

  val genSmallNum = Gen.choose(1, 100)
  def genIndex(e: Traversable[_]) = Gen.choose(0, e.size - 1)

  val curratedBlockSelection = true

  val genPiece = {
    if (curratedBlockSelection)
      Gen.oneOf(PieceKinds.squareStrong, PieceKinds.squareWeak, PieceKinds.largeTriangle, PieceKinds.wedge)
    else
      for (m ← genSmallNum; n ← genSmallNum; s ← genShape) yield PieceKind(s, m, n)
  }
  implicit val arbPiece: Arbitrary[PieceKind] = Arbitrary(genPiece)

  def genAddNode[T <: ShipGraphBase[T]]: Gen[T ⇒ Gen[T]] = for (s ← genPiece; i ← genIndex(s.ports)) yield { g ⇒
    for (portb ← Gen.oneOf(g.disconnectedPorts.toSeq)) yield {
      g.attach(s, i, portb)
    }
  }
  
  def genAddNodeMaybeOnSamePort[T <: ShipGraphBase[T]]: Gen[T ⇒ Gen[T]] = for (s ← genPiece; i ← genIndex(s.ports)) yield { g ⇒
    for (portb ← Gen.oneOf(g.ports.toSeq)) yield {
      g.attach(s, i, portb)
    }
  }
  
  def genCommand[T <: ShipGraphBase[T]]: Gen[T ⇒ Gen[T]] = Gen.oneOf(genAddNode[T], genAddNodeMaybeOnSamePort[T])

  val genShip: Gen[Ship] = {
    Gen.containerOf[Vector, Ship ⇒ Gen[Ship]](genCommand) flatMap { commands ⇒
      commands.foldLeft(Gen.const(Ship()))((s, cmd) ⇒ s.flatMap(cmd(_)))
    }
  }
  implicit val arbGraph: Arbitrary[Ship] = Arbitrary(genShip)
  val genSegment: Gen[ShipSegment] = {
    Gen.containerOf[Vector, ShipSegment ⇒ Gen[ShipSegment]](genCommand) flatMap { commands ⇒
      val g = for (p ← genPiece; pos ← genVec2; r ← Gen.choose(-Pi * 2, Pi * 2)) yield {
        ShipSegment().add(PlacedPiece(Mat3.translate(pos) * Mat3.rotate(r), p)).get
      }
      commands.foldLeft(g)((s, cmd) ⇒ s.flatMap(cmd(_)))
    }
  }
  implicit val arbSegment: Arbitrary[ShipSegment] = Arbitrary(genSegment)
}
