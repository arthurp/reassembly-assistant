package org.singingwizard.reassembly.shipgraphs

import scala.collection.mutable
import Ship._
import org.singingwizard.geo.SpatiallyBinnedSet2
import org.singingwizard.swmath._
import org.singingwizard.geo.AABB2

object SpatiallyBinnedPortSet {
  def apply(size: Int): SpatiallyBinnedSet2[Port] = {
    val range = Vec2(size * 1.9, size * 1.9)
    SpatiallyBinnedSet2[Port](range, size, _.position)
  }
  def apply(): SpatiallyBinnedSet2[Port] = this(11)
}

case class PlacedPiece(t: Mat3, kind: PieceKind) extends GraphNode {
  @inline
  def overlaps(o: PlacedPiece) = (tshape overlaps o.tshape)
  @inline
  def intersects(ray: Ray): Real = {
    //println(s"Itersecting $this X $ray")
    tshape intersects ray
  }

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
  def add(placed: PlacedPiece): Option[ShipT] = {
    // This is lazy because if overlap check fails there is no reason to do it but writing it like this is cleaner.
    lazy val geoEdges = findGeometricallyImpliedEdgesFor(placed)
    if (overlaps(placed) || (!graph.nodes.isEmpty && geoEdges.isEmpty)) {
      None
    } else {
      Some(copy(graph = graph + placed addEdges geoEdges, ports = ports addValues placed.ports))
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

    val visitor = segment.graph.dfsIterator(porta.piece)

    val (gOpt, placed) = visitor.foldLeft[(Option[ShipT], Set[PlacedPiece])]((Some(this), Set())) { (acc, p) ⇒
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
   * O(pieces.size)
   */
  def intersects(r: Ray): Option[PlacedPiece] = {
    val dps = pieces.map(p ⇒ (p, p.intersects(r)))
    //println(dps)
    val (p, d) = dps.minBy(_._2)
    //println(s"Nearest inter: $p@$d")
    if (!d.isInfinite)
      Some(p)
    else
      None
  }

  /**
   * O(ports.size)
   */
  def findGeometricallyImpliedEdgesFor(p: PlacedPiece) = {
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

  def pieces = graph.nodes
  def connections = graph.edges

  def disconnectedPorts = ports.filterNot(p ⇒ graph.incidentEdges(p.piece).exists(_ contains p))

  def pieceCount = pieces.size
  def portCount = ports.size
  def connectionCount = connections.size

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
            p.ports.exists(pp ⇒ r.ports.exists({ pr ⇒
              if (pr.position =~ pp.position) {
                !graph.contains(pp ~ pr)
              } else {
                false
              }
            }))
          }
        }
      }
    }

    def hasInvalidConnection = {
      graph.edges exists { c ⇒ !(c.p1.position =~ c.p2.position) }
    }

    def hasMismatchedPortsAndGraph = {
      val portsContainsExtra = ports exists { p ⇒
        !graph.contains(p.piece)
      }
      val portsHasEverything = graph.nodes forall { piece ⇒
        piece.ports forall { p ⇒
          ports contains p
        }
      }
      portsContainsExtra || !portsHasEverything
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
    } else if (validation.hasUnconnected) Some("Graph contains disconnected pieces")
    else if (validation.hasMissingGeometricEdge) Some("Graph contains touching ports that are not connected by an edge")
    //else if (validation.hasInvalidPieceEdge) Some("Graph contains a piece edge which is not valid w.r.t. it's Piece")
    else if (validation.hasInvalidConnection) Some("Graph contains a connection which is not valid (ports not aligned)")
    else if (validation.hasMismatchedPortsAndGraph) Some(s"The binned ports set and the graph nodes are not the same")
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

final class Ship protected (graph: ShipGraph, ports: SpatiallyBinnedSet2[Port], val core: PlacedPiece)
    extends ShipGraphBase[Ship](graph, ports) {
  override def copy(graph: ShipGraph, ports: SpatiallyBinnedSet2[Port]) = new Ship(graph, ports, core)

  def remove(p: PlacedPiece): Ship = {
    if (p == core)
      return this

    val unreachedPieces = mutable.HashSet() ++ pieces
    graph.dfsIterator(core, _ != p) foreach { p2 ⇒
      unreachedPieces -= p2
    }
    val newg = graph removeNodes unreachedPieces
    copy(graph = newg, ports = SpatiallyBinnedPortSet() addValues newg.nodes.iterator.flatMap(_.ports))
  }

  def piecesWithoutCore = pieces.filter(_ != core)

  val prefixString = "Ship"

  object validationShip {
    def missingCore = {
      !(graph contains core)
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
    val core = PlacedPiece(Mat3.nil, PieceKinds.core)
    val g = ShipGraph() + core
    val ps = SpatiallyBinnedPortSet() addValues core.ports
    new Ship(g, ps, core)
  }

  import scala.language.implicitConversions

  case class Port(id: Shape.PortID, piece: PlacedPiece) {
    def ~(other: Port) = Connection(this, other)
    override def toString = s"${piece.kind}.port($id)@$position"

    val placedPort = piece.tshape.ports(id)

    def position = placedPort.position
    def direction = placedPort.direction
  }

  case class Connection(p1: Port, p2: Port) extends GraphEdge[PlacedPiece] {
    if (!(p1.position =~ p2.position))
      throw new IllegalArgumentException(s"ConnectedPorts can only be matching ports: ${p1.position} != ${p2.position}")

    def contains(p: Port) = p == p1 || p == p2
    override def equals(o: Any) = o match {
      case o: Connection ⇒ (o.p1 == p1 && o.p2 == p2) || (o.p2 == p1 && o.p1 == p2)
      case _ ⇒ false
    }
    override def hashCode() = p1.## ^ p2.##

    def nodes = Set(p1.piece, p2.piece)
  }

  type ShipGraph = Graph[PlacedPiece, Connection]
  def ShipGraph() = Graph[PlacedPiece, Connection]()
}