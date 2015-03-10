package org.singingwizard.reassembly.shipgraphs

import org.specs2._
import org.specs2.runner.JUnitRunner
import org.singingwizard.swmath._
import org.junit.runner.RunWith
import org.scalacheck._
import scalaz._
import scalax.collection.GraphEdge.EdgeException

@RunWith(classOf[JUnitRunner])
class GraphSpec extends mutable.Specification {
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
  
  "A ship graph" >> {
    "Empty has one node" >> {
      Ship().pieceCount must_== 1
      Ship().pieces must have size(1)
    }
    "Adding one node should have 2 nodes" >> {
      val s1 = Ship()
      val s2 = s1.attach(PieceKinds.squareWeak, 0, s1.core.ports(0))
      s2.pieceCount must_== 2
      s2.pieces must have size(2)
    }
    "Nodes should be connected both ways" >> {
      val s1 = Ship()
      val (s2, n) = s1.attachGet(PieceKinds.squareWeak, 0, s1.core.ports(0))
      s2.pieceCount must_== 2
      s2.pieces must have size(2)
      s2.connectionCount must_== 1
      s2.connections must have size(1)
      val c = s2.connections.head
      c._1.id ==== 0
      c._2.id ==== 0
      c.isAt(n.ports(0)) ==== true
      c.isAt(s2.core.ports(0)) ==== true
    }
    /*"Generator test" >> {
      GraphSpec.genGraph.sample
      true
    }*/
  }
}

/*
object GraphSpec {
  val genShape = Gen.oneOf(
      Shapes.square, 
      Shapes.rightTriangle, 
      Shapes.longTriangle, 
      Shapes.longTriangle.flipped, 
      Shapes.octogon, 
      Shapes.smallRectangle, 
      Shapes.equilateralTriangle,
      Shapes.regularPolygon(6)
      )
  val genAddNode: Gen[State[Graph, Unit]] = for( s <- genShape ) yield for (_ <- Graph.addNode(s)) yield ()
  val genConnectPorts: Gen[State[Graph, Unit]] = 
    for( a <- Gen.posNum[Int]; b <- Gen.posNum[Int]; ap <- Gen.posNum[Int]; bp <- Gen.posNum[Int] ) 
      yield for {
      nNodes <- State.gets((_:Graph).nodes.size)
      an <- State.gets((_:Graph).nodes(a % nNodes))
      bn <- State.gets((_:Graph).nodes(b % nNodes))
      porta = an.ports(ap % an.ports.size)
      portb = bn.ports(bp % bn.ports.size)
      _ <- State.modify { (g: Graph) =>
        if (an != bn && porta != portb && g.connectedEdge(porta).isEmpty && g.connectedEdge(portb).isEmpty)
          g.connectPorts(porta, portb)
        else
          g
      }
    } yield ()
  val genChange = Gen.frequency((1, genAddNode), (2, genConnectPorts))
  
  val genGraph: Gen[Graph] = {
    for( commands <- Gen.containerOf[Vector, State[Graph, Unit]](genChange) ) yield
      commands.foldLeft(Graph.empty)((g, cmd) => cmd exec g) 
  }
  implicit val arbGraph: Arbitrary[Graph] = Arbitrary(genGraph)
}

*/