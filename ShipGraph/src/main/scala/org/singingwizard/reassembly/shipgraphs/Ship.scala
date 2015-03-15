package org.singingwizard.reassembly.shipgraphs

import scala.collection.mutable
import scalax.collection.immutable.Graph
import scalax.collection.edge._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import org.singingwizard.swmath.Mat3
import Ship._
import org.singingwizard.geo.SpatiallyBinnedSet2
import org.singingwizard.swmath.Vec2
import org.singingwizard.geo.AABB2

object SpatiallyBinnedPortSet {
  def apply(size: Int): SpatiallyBinnedSet2[Port] = {
    val range = Vec2(size * 1.9, size * 1.9)
    SpatiallyBinnedSet2[Port](range, size, _.position)
  }
  def apply(): SpatiallyBinnedSet2[Port] = this(11)
}

case class PlacedPiece(t: Mat3, kind: PieceKind) {
  def overlaps(o: PlacedPiece) = (boundingbox overlaps o.boundingbox) && (tshape overlaps o.tshape)

  def shape = kind.shape
  val tshape = shape.transform(t)
  
  val boundingbox = AABB2(tshape.vertices)

  val ports = (0 until kind.ports.size).map(Ship.Port(_, this)).toIndexedSeq

  def transform(t2: Mat3) = PlacedPiece(t * t2, kind)

  override def toString = s"$kind@${tshape.centroid}"

  override lazy val hashCode = t.hashCode ^ kind.hashCode

  override def equals(o: Any): Boolean = o match {
    case PlacedPiece(ot, ok) ⇒ {
      t == ot && kind == ok
    }
    case _ ⇒ false
  }
}

abstract class ShipGraphBase[ShipT <: ShipGraphBase[ShipT]] protected (val graph: Graph[Port, Edge], val ports: SpatiallyBinnedSet2[Port]) {
  this: ShipT ⇒
  import Ship._

  /**
   * O(pieces.size + ports.size)
   */
  def add(placed: org.singingwizard.reassembly.shipgraphs.PlacedPiece): Option[ShipT] = {
    val geoEdges = findGeometricallyImpliedEdgesFor(placed)
    if (overlaps(placed) || (!graph.nodes.isEmpty && geoEdges.isEmpty)) {
      None
    } else {
      Some(copy(graph = graph + placed ++ geoEdges, ports = ports addValues placed.ports))
    }
  }

  def attach(segment: ShipSegment, porta: Port, portb: Port, allowPartial: Boolean = false) = attachGet(segment, porta, portb, allowPartial)._1
  def attachGet(segment: ShipSegment, porta: Port, portb: Port, allowPartial: Boolean = false): (ShipT, Set[PlacedPiece]) = {
    require(segment.ports contains porta)
    require(ports contains portb)
    val portaPlace = porta.placedPort
    val portbPlace = portb.placedPort
    val trans = portbPlace.matchingTransform(portaPlace)

    /*
    println(porta, portb)
    println(portaPlace, portbPlace)
    println(portaPlace.transform(trans), portbPlace)
    println(this)
    println(segment)
    println(trans)
    */

    import scalax.collection.GraphTraversal._
    val visitor = segment.graph.get(porta.piece).nodes.head.outerEdgeTraverser.withKind(DepthFirst)

    val visited = mutable.Set[PlacedPiece]()

    val (gOpt, placed) = visitor.foldLeft[(Option[ShipT], Set[PlacedPiece])]((Some(this), Set())) { (acc, p) ⇒
      p match {
        case Piece(p) if !visited.contains(p) ⇒
          visited += p
          val (g, placed) = acc
          val pt = p.transform(trans)
          //println(s"adding $pt")
          val r = g.flatMap(_.add(pt))
          //println(r)
          r match {
            case Some(r) ⇒ (Some(r), placed)
            case None if allowPartial ⇒ (g, placed + pt)
            case None ⇒ (None, placed)
          }
        case Piece(_) | Connection(_, _) ⇒ acc
      }
    }

    gOpt match {
      case Some(g) ⇒ (g, placed)
      case None ⇒ (this, segment.pieces.map(_.transform(trans)).toSet)
    }
  }

  def attach(p: PieceKind, portaID: Shape.PortID, portb: Port) = attachGet(p, portaID, portb)._1
  def attachGet(p: PieceKind, portaID: Shape.PortID, portb: Port): (ShipT, Option[PlacedPiece]) = {
    val porta = p.shape.ports(portaID)
    val portbPlace = portb.placedPort
    val trans = portbPlace.matchingTransform(porta)
    val placed = PlacedPiece(trans, p)
    add(placed) match {
      case Some(g) ⇒ (g, Some(placed))
      case None ⇒ (this, None)
    }
  }

  var overlapChecks = 0
  var portAlignmentChecks = 0

  /**
   * O(pieces.size)
   */
  def overlaps(p: PlacedPiece) = {
    // TODO: Actually select only near by pieces. This will require some sort of search structure for pieces not points.
    val nearPieces = pieces 
    //overlapChecks += nearPieces.size
    nearPieces.exists(_.overlaps(p))
  }

  /**
   * O(ports.size)
   */
  def findGeometricallyImpliedEdgesFor(p: PlacedPiece) = {
    for {
      pp ← p.ports
      pr ← ports(pp.position) 
      //_ <- Some(portAlignmentChecks += 1)
      if pr.position =~ pp.position
    } yield pp ~ pr
  }

  def copy(graph: Graph[Port, Edge], ports: SpatiallyBinnedSet2[Port]): ShipT

  val prefixString: String
  override def toString = {
    s"$prefixString(${pieces.mkString(", ")}; ${connections.mkString(", ")})"
  }

  def get(p: PlacedPiece) = graph.get(p).edge.asInstanceOf[Piece[_]]
  def get(p: Port) = graph.get(p)
  def get(c: Connection[Port]) = graph.get(c).edge.asInstanceOf[Connection[_]]
  //def get(c: Piece[Port]) = graph.get(c)

  def pieces = graph.edges.map(_.edge).collect({ case p: Piece[_] ⇒ p.piece })

  def connections = graph.edges.map(_.edge).collect({ case c: Connection[_] ⇒ c })
  //def graphPorts = graph.nodes.toOuter

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

    def hasInvalidPieceEdge = {
      graph.edges exists { edge ⇒
        edge.edge match {
          case edge @ Piece(piece) ⇒ {
            val b = (edge.ports.exists(_.piece != piece) ||
              piece.ports.map(_.id).toSeq.sorted != (0 until piece.shape.ports.size).toSeq)
            b
          }
          case _ ⇒ false
        }
      }
    }

    def hasInvalidConnection = {
      graph.edges exists { edge ⇒
        edge.edge match {
          case Connection(p1, p2) ⇒ !(p1.position =~ p2.position) && !(p1.direction =~ -p2.direction)
          case _ ⇒ false
        }
      }
    }

    def hasMismatchedPortsAndGraph = {
      (graph.nodes exists { n ⇒
        !(ports contains n.value)
      }) ||
        (ports.values exists { n ⇒
          !(graph contains n)
        })
    }
  }

  def validate() = {
    if (validation.hasOverlaps) {
      var overlappingPair: (PlacedPiece, PlacedPiece) = null
      pieces.tails exists { ps ⇒
        if (ps.isEmpty) {
          false
        } else {
          val p = ps.head
          val rest = ps.tail
          rest.exists { r ⇒
            overlappingPair = (r, p)
            r.overlaps(p)
          }
        }
      }
      val (r, p) = overlappingPair
      Some(s"Graph contains overlapping pieces: $r ${r.t}; $p ${p.t}")
    } else if (validation.hasUnconnected) Some("Graph contains has pieces that are not connected to the core")
    else if (validation.hasMissingGeometricEdge) Some("Graph contains touching ports that are not connected by an edge")
    else if (validation.hasInvalidPieceEdge) Some("Graph contains a piece edge which is not valid w.r.t. it's Piece")
    else if (validation.hasInvalidConnection) Some("Graph contains a connection which is not valid (ports not aligned)")
    else if (validation.hasMismatchedPortsAndGraph) Some(s"The binned ports set and the graph nodes are not the same: ${graph.nodes.toOuterNodes.toSet} != ${ports.values}")
    else None
  }
}

class ShipSegment protected (graph: Graph[Port, Edge], ports: SpatiallyBinnedSet2[Port])
    extends ShipGraphBase[ShipSegment](graph, ports) {

  def copy(graph: Graph[Port, Edge], ports: SpatiallyBinnedSet2[Port]): ShipSegment = new ShipSegment(graph, ports)

  val prefixString = "Segment"
}

object ShipSegment {
  def apply(): ShipSegment = {
    val g = Graph[Port, Edge]()
    new ShipSegment(g, SpatiallyBinnedPortSet())
  }
}

final class Ship protected (graph: Graph[Port, Edge], ports: SpatiallyBinnedSet2[Port], corePiece: Piece[Port])
    extends ShipGraphBase[Ship](graph, ports) {
  def core = corePiece.piece
  override def copy(graph: Graph[Port, Edge], ports: SpatiallyBinnedSet2[Port]) = new Ship(graph, ports, corePiece)

  val prefixString = "Ship"

  object validationShip {
    def missingCore = {
      !(graph contains corePiece)
    }
  }

  override def validate() = {
    if (validationShip.missingCore) Some("Graph does not contain the ship core")
    else super.validate()
  }
}

object Ship {
  def apply(): Ship = {
    val core: Piece[Port] = PlacedPiece(Mat3.nil, PieceKinds.core)
    val g = Graph[Port, Edge]() + core
    val ps = SpatiallyBinnedPortSet() addValues core.ports
    new Ship(g, ps, core)
  }

  import scala.language.implicitConversions

  case class Port(id: Shape.PortID, piece: PlacedPiece) {
    def ~(other: Port) = Connection(this, other)
    override def toString = s"$piece.port($id)"

    val placedPort = piece.tshape.ports(id)

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
      if (e eq null) None else Some((portFromNode(e._1), portFromNode(e._2)))
  }
}