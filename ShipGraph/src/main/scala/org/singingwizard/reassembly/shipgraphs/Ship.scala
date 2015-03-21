package org.singingwizard.reassembly.shipgraphs

import scala.collection.mutable
import scalax.collection.Graph
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
  def overlaps(o: PlacedPiece) = (tshape overlaps o.tshape)

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

abstract class ShipGraphBase[ShipT <: ShipGraphBase[ShipT]] protected (val graph: ShipGraph, val ports: SpatiallyBinnedSet2[Port]) {
  this: ShipT ⇒
  import Ship._

  /**
   * O(pieces.size + ports.size)
   */
  def add(placed: org.singingwizard.reassembly.shipgraphs.PlacedPiece): Option[ShipT] = {
    // This is lazy because if overlap check fails there is no reason to do it but writing it like this is cleaner.
    lazy val geoNodes = findGeometricallyImpliedNodesFor(placed)
    if (overlaps(placed) || (!graph.nodes.isEmpty && geoNodes.isEmpty)) {
      None
    } else {
      val edge = Piece(geoNodes)
      Some(copy(graph = graph + edge, ports = ports addValues placed.ports))
    }
  }

  def attach(segment: ShipSegment, porta: Port, portb: Port, allowPartial: Boolean = false) = attachGet(segment, porta, portb, allowPartial)._1
  /**
   * Attach the segment to this ship. Return the new ship and the pieces that could not be attached.
   */
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
        case Piece(_) ⇒ acc
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
  def findGeometricallyImpliedNodesFor(p: PlacedPiece) = {
    for {
      pp ← p.ports
      pr ← ports(pp.position)
      //_ <- Some(portAlignmentChecks += 1)
      if pr.position =~ pp.position && pr.piece != pp.piece
    } yield pp ~ pr
  }

  def copy(graph: ShipGraph, ports: SpatiallyBinnedSet2[Port]): ShipT

  val prefixString: String
  override def toString = {
    s"$prefixString(${pieces.mkString(", ")}; ${connections.mkString(", ")})"
  }

  def get(p: PlacedPiece) = graph.get(p).edge.asInstanceOf[Piece[_]]
  def get(p: ConnectedPorts) = graph.get(p)

  def pieces = graph.edges.map(_.edge.piece)
  def connections = graph.nodes.map(_.value).collect({
    case p: ConnectedPorts ⇒ p
  })

  def disconnectedPorts = graph.nodes.map(_.value).collect({
    case DisconnectedPort(p1) ⇒ p1
  })

  def pieceCount = graph.graphSize
  def portCount = graph.nodes.iterator.map(_.value).collect({
    case p: ConnectedPorts ⇒ 2
    case DisconnectedPort(p1) ⇒ 1
  }).sum
  def connectionCount = graph.nodes.iterator.map(_.value).count(_.isInstanceOf[ConnectedPorts])

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
          case Piece(piece) ⇒ {
            val b = (piece.ports.exists(_.piece != piece) ||
              piece.ports.map(_.id).toSeq.sorted != (0 until piece.shape.ports.size).toSeq)
            b
          }
          case _ ⇒ false
        }
      }
    }

    def hasInvalidConnection = {
      graph.nodes.map(_.value) exists {
        case ConnectedPorts(p1, p2) ⇒ p1.position =~ p2.position
        case _ ⇒ false
      }
    }

    def hasMismatchedPortsAndGraph = {
      (graph.nodes.map(_.value) exists {
        case ConnectedPorts(p1, p2) ⇒ !(ports contains p1) || !(ports contains p2)
        case DisconnectedPort(p1) ⇒ !(ports contains p1)
      }) ||
        (ports.values exists { n ⇒
          !graph.nodes.exists(_ contains n)
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

class ShipSegment protected (graph_ : ShipGraph, ports: SpatiallyBinnedSet2[Port])
    extends ShipGraphBase[ShipSegment](graph_, ports) {

  def copy(graph: ShipGraph, ports: SpatiallyBinnedSet2[Port]): ShipSegment = new ShipSegment(graph, ports)

  val prefixString = "Segment"

  override lazy val hashCode = graph.hashCode

  override def equals(o: Any): Boolean = o match {
    case o: ShipSegment ⇒ {
      graph == o.graph
    }
    case _ ⇒ false
  }
}

object ShipSegment {
  def apply(): ShipSegment = {
    val g = ShipGraph()
    new ShipSegment(g, SpatiallyBinnedPortSet())
  }
}

final class Ship protected (graph: ShipGraph, ports: SpatiallyBinnedSet2[Port], corePiece: Piece[PortPoint])
    extends ShipGraphBase[Ship](graph, ports) {
  def core = corePiece.piece
  override def copy(graph: ShipGraph, ports: SpatiallyBinnedSet2[Port]) = new Ship(graph, ports, corePiece)

  def remove(p: PlacedPiece): Ship = {
    if (p == core)
      return this

    val newg = graph - p -- p.ports.map(DisconnectedPort(_): PortPoint)
    ???
    /*val newg2 = newg.innerElemTraverser(newg.get(core.ports(0))).toGraph
    copy(graph = newg2, ports = SpatiallyBinnedPortSet() addValues newg2.edges.collect({
      case Piece(p) ⇒ p.ports
    }).flatten)*/
  }

  def piecesWithoutCore = pieces.filter(_ != core)

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

  override lazy val hashCode = graph.hashCode ^ core.hashCode

  override def equals(o: Any): Boolean = o match {
    case o: Ship ⇒ {
      graph == o.graph && core == o.core
    }
    case _ ⇒ false
  }
}

object Ship {
  def apply(): Ship = {
    val core: Piece[PortPoint] = PlacedPiece(Mat3.nil, PieceKinds.core)
    val g = ShipGraph() + core
    val ps = SpatiallyBinnedPortSet() addValues core.ports.map(_.p1)
    new Ship(g, ps, core)
  }

  import scala.language.implicitConversions

  case class Port(id: Shape.PortID, piece: PlacedPiece) {
    def ~(other: Port) = ConnectedPorts(this, other)
    override def toString = s"${piece.kind}.port($id)@$position"

    val placedPort = piece.tshape.ports(id)

    def position = placedPort.position
    def direction = placedPort.direction
  }
  abstract class PortPoint(val p1: Port) {
    def position = p1.position
    def toSet: Set[Port]
    def contains(o: Port) = toSet contains o

    override def equals(o: Any) = o match {
      case o: PortPoint ⇒ position =~ o.position
      case _ ⇒ false
    }
    override def hashCode() = 0 // Horrible hack. This will make hash tables degrade to linear search.
  }
  case class ConnectedPorts(override val p1: Port, p2: Port) extends PortPoint(p1) {
    if (!(p1.position =~ p2.position))
      throw new IllegalArgumentException(s"ConnectedPorts can only be matching ports: ${p1.position} != ${p2.position}")
    def toSet: Set[Port] = Set(p1, p2)
  }
  case class DisconnectedPort(override val p1: Port) extends PortPoint(p1) {
    def toSet: Set[Port] = Set(p1)
  }

  trait SupportNodesIterator[N] {
    this: EdgeLike[N] ⇒
    def nodesIterator: Iterator[N] = convertNodesProduct(nodes)
  }

  private def buildNodesFromShape(piece: PlacedPiece): Iterable[PortPoint] = {
    (0 until piece.shape.ports.size).map(i ⇒ DisconnectedPort(Port(i, piece))).toSeq
  }
  private def portFromNode(a: Any) = a match {
    case in: Graph[_, _]#InnerNode ⇒ in.value.asInstanceOf[PortPoint]
    case p: PortPoint ⇒ p
  }
  private def convertNodesProduct[N](nodes: Product) = nodes match {
    case i: Iterable[N] ⇒ i.iterator
    case p: Product ⇒ p.productIterator.asInstanceOf[Iterator[N]]
  }

  class Piece[N](_nodes: Product, val piece: PlacedPiece)
      extends LkHyperEdge[N](_nodes)
      with LoopFreeEdge[N]
      with EdgeCopy[Piece]
      with OuterEdge[N, Piece]
      with SupportNodesIterator[N] {
    val ports = nodesIterator.map(portFromNode).toIndexedSeq
    type L1 = PlacedPiece
    override val label = piece

    override def copy[NN](newNodes: Product) = {
      val piece: PlacedPiece = {
        val s = convertNodesProduct[NN](newNodes).map(n => portFromNode(n).toSet.map(_.piece)).reduce(_ & _)
        require(s.size == 1, (s, newNodes, convertNodesProduct[NN](newNodes).map(n => portFromNode(n).toSet.map(_.piece)).toList))
        s.head
      }
      new Piece[NN](newNodes, piece)
    }

    override def toString = {
      piece.toString
    }
  }
  object Piece {
    def apply(specificPorts: Iterable[PortPoint]) = {
      require(specificPorts.size > 0)
      val piece: PlacedPiece = {
        val s = specificPorts.iterator.map(_.toSet.map(_.piece)).reduce(_ & _)
        require(s.size == 1)
        s.head
      }
      val shapePorts = buildNodesFromShape(piece)
      val ports = shapePorts.filterNot(specificPorts.toSet contains _) ++ specificPorts
      val nodes = NodeProduct(ports)
      new Piece[PortPoint](nodes, piece)
    }
    def apply(piece: PlacedPiece) = {
      val ports = buildNodesFromShape(piece)
      val nodes = NodeProduct(ports)
      new Piece[PortPoint](nodes, piece)
    }
    def unapply[N](e: Piece[N]): Option[PlacedPiece] = Some(e.piece)
    def unapply[N](e: Graph[N, Piece]#InnerEdge): Option[PlacedPiece] = Some(e.edge.piece)
  }
  implicit def PieceConvert(piece: PlacedPiece): Piece[PortPoint] = Piece(piece)

  type ShipGraph = Graph[PortPoint, Piece]
  def ShipGraph() = Graph[PortPoint, Piece]()
}