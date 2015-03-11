package org.singingwizard.reassembly.shipgraphs

import scalax.collection.immutable.Graph
import scalax.collection.edge._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import org.singingwizard.swmath.Mat3

import Ship._

case class PlacedPiece(transform: Mat3, kind: PieceKind) {
  def overlaps(o: PlacedPiece) = tshape overlaps o.tshape

  def shape = kind.shape
  val ports = (0 until kind.ports.size).map(Ship.Port(_, this)).toIndexedSeq

  lazy val tshape = shape.transform(transform)

  override def toString = s"$kind@${tshape.centroid}"
}

class Ship private (val graph: Graph[Port, Edge], corePiece: Piece[Port]) {
  def core = corePiece.piece

  assert(graph contains corePiece)

  def attach(p: PieceKind, portaID: Shape.PortID, portb: Port) = attachGet(p, portaID, portb)._1
  def attachGet(p: PieceKind, portaID: Shape.PortID, portb: Port): (Ship, Option[PlacedPiece]) = {
    val porta = p.shape.ports(portaID)
    val portbPlace = portb.piece.tshape.ports(portb.id)
    val trans = portbPlace.matchingTransform(porta)
    val placed = PlacedPiece(trans, p)
    assert(placed.tshape.ports(portaID).position =~ portbPlace.position)
    if (overlaps(placed)) {
      (this, None)
    } else {
      (copy(graph = graph + placed ++ findGeometricallyImpliedEdgesFor(placed)), Some(placed))
    }
  }

  def overlaps(p: PlacedPiece) = pieces.exists(_.overlaps(p))
  
  /**
   * O(ports.size)
   */
  def findGeometricallyImpliedEdgesFor(p: PlacedPiece) = {
    for {
      pr <- ports
      pp <- p.ports
      if pr.position =~ pp.position
    } yield pp ~ pr
  }

  def copy(graph: Graph[Port, Edge] = graph, corePiece: Piece[Port] = corePiece) =
    new Ship(graph, corePiece)

  override def toString = {
    s"Ship(${pieces.mkString(", ")}; ${connections.mkString(", ")})"
  }

  def get(p: PlacedPiece) = graph.get(p)
  def get(p: Port) = graph.get(p)
  def get(c: Connection[Port]) = graph.get(c)
  //def get(c: Piece[Port]) = graph.get(c)

  def pieces = graph.edges.map(_.edge).collect({ case p: Piece[_] ⇒ p.piece })
  def connections = graph.edges.map(_.edge).collect({ case c: Connection[_] ⇒ c })
  def ports = graph.nodes.map(_.value)

  def disconnectedPorts = graph.nodes.filter(_.edges.size == 1).map(_.value)

  def pieceCount = graph.edges.count(_.edge.isInstanceOf[Piece[_]])
  def connectionCount = graph.edges.count(_.edge.isInstanceOf[Connection[_]])
  def portCount = graph.order

  object validation {
    def hasOverlaps = {
      pieces.tails exists { ps ⇒
        if (ps.isEmpty) {
          false
        } else {
          val p = ps.head
          val rest = ps.tail
          rest.exists(_.overlaps(p))
        }
      }
    }
    def hasUnconnected = !graph.isConnected
    def hasMissingGeometricEdge = {
      pieces.tails exists { ps ⇒
        if (ps.isEmpty) {
          false
        } else {
          val p: PlacedPiece = ps.head
          val rest = ps.tail
          // Is there any piece in rest 
          rest exists { (r: PlacedPiece) ⇒
            // such that some port on p that has the same position as some port on r that are not connected?
            p.ports.exists(pp ⇒ r.ports.exists(pr ⇒ pr.position =~ pp.position && !graph.contains(pp ~ pr)))
          }
        }
      }
    }
  }

  def validate() = {
    if (validation.hasOverlaps) Some("Graph contains overlapping pieces")
    else if (validation.hasUnconnected) Some("Graph contains has pieces that are not connected to the core")
    else if (validation.hasMissingGeometricEdge) Some("Graph contains touching ports that are not connected by an edge")
    else None
  }
}

object Ship {

  def apply(): Ship = {
    val core: Piece[Port] = PlacedPiece(Mat3.nil, PieceKinds.core)
    val g = Graph[Port, Edge]() + core
    new Ship(g, core)
  }

  import scala.language.implicitConversions

  case class Port(id: Shape.PortID, piece: PlacedPiece) {
    def ~(other: Port) = Connection(this, other)
    override def toString = s"$piece.port($id)"

    lazy val placedPort = piece.tshape.ports(id)

    def position = placedPort.position
    def direction = placedPort.direction
  }

  sealed class Edge[N](nodes: Product, weight: Long)
      extends WHyperEdge[N](nodes, weight)
      with LoopFreeEdge[N] {
  }

  trait SupportNodesIterator[N] {
    this: EdgeLike[N] ⇒
    def nodesIterator: Iterator[N] = nodes match {
      case i: Iterable[N] ⇒ i.iterator
      case p: Product ⇒ p.productIterator.asInstanceOf[Iterator[N]]
    }
  }

  private def buildNodesFromShape(piece: PlacedPiece): Product = {
    NodeProduct((0 until piece.shape.ports.size) map { Port(_, piece) })
  }
  private def portFromNode(a: Any) = a match {
    case in: Graph[_, _]#InnerNode ⇒ in.value.asInstanceOf[Port]
    case p: Port ⇒ p
  }

  class Piece[N](_nodes: Product, val piece: PlacedPiece)
      extends Edge[N](_nodes, piece.kind.hitpoints)
      with EdgeCopy[Piece]
      with OuterEdge[N, Piece]
      with SupportNodesIterator[N] {
    val ports = nodesIterator.map(portFromNode).toIndexedSeq
    assert(ports.forall(_.piece == piece))
    assert(ports.map(_.id).toSeq.sorted == (0 until piece.shape.ports.size).toSeq)

    override def copy[NN](newNodes: Product) = {
      // Hack to get the piece of one of the port nodes
      val newPiece = portFromNode(newNodes.productElement(0)).piece
      new Piece[NN](newNodes, newPiece)
    }

    override def toString = {
      piece.toString
    }
  }
  object Piece {
    def apply(piece: PlacedPiece) = new Piece[Port](buildNodesFromShape(piece), piece)
    def unapply(e: Piece[_]): Option[PlacedPiece] =
      if (e eq null) None else Some(e.piece)
  }
  implicit def PieceConvert(piece: PlacedPiece): Piece[Port] = Piece(piece)

  class Connection[N](_nodes: Product)
      extends Edge[N](_nodes, Long.MaxValue)
      with EdgeCopy[Connection]
      with OuterEdge[N, Connection] {
    def isValidLoopBack = portFromNode(this._1).piece != portFromNode(this._2).piece
    override def isValidCustom = isValidLoopBack && super.isValidCustom
    override def isValidCustomExceptionMessage = {
      if (!isValidLoopBack)
        "This edge is a loop back to the same piece"
      else
        super.isValidCustomExceptionMessage
    }
    override val size = 2
    override def copy[NN](newNodes: Product) = new Connection[NN](newNodes)
    override def toString = s"${_1} ~ ${_2}"
  }
  object Connection {
    def apply(a: Port, b: Port) = new Connection[Port](NodeProduct(a, b))
    def unapply[A <: Port](e: Connection[A]): Option[(Port, Port)] =
      if (e eq null) None else Some((e._1, e._2))
  }
}