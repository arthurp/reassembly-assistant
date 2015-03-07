package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.reassembly.geometry._

case class Shape()

abstract class Graph {
  abstract class Node(shape: Shape) {
    case class Port(position: Position) {
      def node = Node.this
    }
    
    val ports: Set[Port]
  }
  
  case class Edge(a: Node#Port, b: Node#Port) {
    assert(a != b)
    assert(a.node != b.node)
    
    override def equals(o: Any) = o match {
      case e: Edge => (e.a == a && e.b == b) || (e.a == b && e.b == a)
      case _ => false
    }
    override def hashCode() = a.hashCode() ^ b.hashCode()
  }
  
  val edges: Set[Edge]
  val nodes: Set[Node]
}

object Graph {
  val empty = new Graph {
    val edges = Set[Edge]()
    val nodes = Set[Node]() 
  }
}