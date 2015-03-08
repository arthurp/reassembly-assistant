package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath._
import scalaz.State
import scala.collection.mutable

abstract class Graph {
  import Graph._

  val anchor: Node

  val edges: Set[Edge]
  val nodes: Map[NodeID, Node]

  def lookup(id: NodeID): Node = nodes(id)
  def lookup(id: PortID): Node#Port = lookup(id.node).ports(id.portID)

  def connectedEdge(p: Node#Port): Option[Edge] = {
    val s = edges.filter(_.connects(p.id))
    assert(s.size <= 1)
    s.headOption
  }

  def connectedPort(p: Node#Port): Option[Node#Port] = {
    val s = edges.flatMap(_.getOther(p.id))
    assert(s.size <= 1)
    s.headOption.map(lookup(_))
  }

  def connectPorts(a: Node#Port, b: Node#Port): Graph = {
    val oldThis = this
    new Graph {
      val anchor = oldThis.anchor
      val edges = oldThis.edges + Edge(a.id, b.id)
      val nodes = oldThis.nodes
    }
  }

  private lazy val nextID = nodes.keys.max + 1

  def addNode(shape: Shape): (Graph, Node) = {
    val oldThis = this
    val n = Node(nextID, shape)
    (new Graph {
      val anchor = oldThis.anchor
      val edges = oldThis.edges
      val nodes = oldThis.nodes + (n.id -> n)
    }, n)
  }

  def removeUnanchored() = {
    val oldThis = this
    new Graph {
      val anchor = oldThis.anchor
      val nodes = oldThis.connectedComponents().head.map(n => n.id -> n).toMap
      val edges = oldThis.edges.filter(e => nodes.keySet.exists(e.connects(_)))
    }
  }
  
  def connectedComponents(): Iterable[Set[Node]] = {
    val remainingNodes = mutable.Set[Node]() ++ nodes.values
    
    def stream(startNode: Node): Stream[Set[Node]] = {
      val foundNodes = mutable.Set[Node]() 
      def visit(n: Node): Unit = {
        if (remainingNodes.contains(n)) {
          remainingNodes -= n
          foundNodes += n
          for {
            outp ← n.ports.toSet[Node#Port]
            inp ← connectedPort(outp)
          } visit(inp.node)
        }
      }
      visit(startNode)
      foundNodes.toSet #:: (remainingNodes.headOption match {
        case Some(n) => stream(n)
        case None => Stream.empty
      })
    }
    
    stream(anchor)
  }

  override def toString: String = {
    s"Anchor: $anchor\nNodes:\n${nodes.values.toSeq.sortBy(_.id).mkString("\n")}\nEdges:\n${edges.toSeq.sortBy(_.a.node).mkString("\n")}"
  }

  override def equals(o: Any) = o match {
    case o: Graph ⇒
      nodes == o.nodes &&
        edges == o.edges &&
        anchor == o.anchor
    case _ ⇒ false
  }
}

object Graph {
  type NodeID = Int
  case class PortID(node: NodeID, portID: Int)

  case class Node(id: NodeID, shape: Shape) {
    case class Port(portID: Int) {
      def id = PortID(Node.this.id, portID)
      def node = Node.this
      def placement = shape.ports(portID)
    }

    def vertices = shape.vertices

    val ports: IndexedSeq[Port] = for ((p, i) ← shape.ports.zipWithIndex) yield Port(i)

    def transform(trans: Mat3): Node = Node(id, shape.transform(trans))
  }

  case class Edge(a: PortID, b: PortID) {
    assert(a != b)
    assert(a.node != b.node)

    def getOther(v: PortID): Option[PortID] = {
      if (a == v)
        Some(b)
      else if (b == v)
        Some(a)
      else None
    }

    def connects(v: PortID): Boolean = v == b || v == a
    def connects(v: NodeID): Boolean = v == b.node || v == a.node

    override def equals(o: Any) = o match {
      case e: Edge ⇒ (e.a == a && e.b == b) || (e.a == b && e.b == a)
      case _ ⇒ false
    }
    override def hashCode() = a.hashCode() ^ b.hashCode()
  }

  val empty: Graph = new Graph {
    val anchor = Node(0, Shapes.square)
    val edges = Set[Edge]()
    val nodes = Map[NodeID, Node]() + (0 -> anchor)
  }

  def addNode(s: Shape) = State[Graph, Node] { _.addNode(s) }
  def connectPorts(a: Node#Port, b: Node#Port) = State[Graph, Unit] { g ⇒ (g.connectPorts(a, b), ()) }
  def getAnchor = State[Graph, Node] { g ⇒ (g, g.anchor) }
}