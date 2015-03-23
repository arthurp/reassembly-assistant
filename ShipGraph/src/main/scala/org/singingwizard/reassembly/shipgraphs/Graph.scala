package org.singingwizard.reassembly.shipgraphs

import scala.collection.AbstractIterator
import scala.collection.mutable

trait GraphEdge[N] {
  def nodes: Set[N]
}

trait GraphNode {
  final def edges[N >: this.type <: GraphNode, E <: GraphEdge[N]](implicit g: Graph[N, E]): Set[E] = g.incidentEdges(this)
}

class Graph[N <: GraphNode, E <: GraphEdge[N]] protected (val edges: Set[E], private val nodeMap: Map[N, Set[E]]) {
  graph ⇒
  def +(n: N) = new Graph(edges, nodeMap + (n -> Set[E]()))
  def addNodes(ns: Iterable[N]) = new Graph(edges, nodeMap ++ ns.map(_ -> Set[E]()))
  def -(n: N) = {
    val removedEdges = incidentEdges(n)
    new Graph(edges -- removedEdges, removedEdges.foldLeft(nodeMap)(Graph.nodeMapWithout) - n)
  }
  def removeNodes(ns: Iterable[N]) = {
    val removedEdges = ns.flatMap(incidentEdges)
    new Graph(edges -- removedEdges, removedEdges.foldLeft(nodeMap)(Graph.nodeMapWithout) -- ns)
  }

  def +(e: E) = new Graph(edges + e, Graph.nodeMapWith(nodeMap, e))
  def addEdges(es: Iterable[E]) = new Graph(edges ++ es, es.foldLeft(nodeMap)(Graph.nodeMapWith))
  def -(e: E) = new Graph(edges - e, Graph.nodeMapWithout(nodeMap, e))

  def nodes = nodeMap.keySet

  def incidentEdges(n: N) = nodeMap(n)
  def neighbors(n: N) = nodeMap(n).flatMap(_.nodes) - n

  def isConnected = {
    if (nodes.isEmpty) {
      true
    } else {
      dfsIterator(nodes.head).size == nodes.size
    }
  }

  def contains(e: E) = edges contains e
  def contains(n: N) = nodes contains n

  protected def simpleIterator(startingNodes: Graph.ElementOrderer[N]): Iterator[N] = new AbstractIterator[N] {
    val storedNodes = startingNodes
    val storedNodeSet = new mutable.HashSet[N]()

    def hasNext = !storedNodes.isEmpty
    def next() = {
      val n = storedNodes.get()
      storedNodeSet += n // TODO: This is only needed at the beginning. Only do it then.
      for (n2 ← graph.incidentEdges(n).iterator.flatMap(_.nodes) if !storedNodeSet.contains(n2)) {
        storedNodeSet += n2
        storedNodes.put(n2)
      }
      n
    }
  }

  def bfsIterator(n: N): Iterator[N] = simpleIterator(new Graph.ElementOrderer[N] {
    val storedNodes = new mutable.Queue[N]()
    storedNodes += n

    def put(e: N): Unit = storedNodes += e
    def get() = storedNodes.dequeue()
    def isEmpty: Boolean = storedNodes.isEmpty
  })

  def bfsPath(source: N, sink: N, edgeFilter: (N, N) ⇒ Boolean): Option[Seq[N]] = {
    val storedNodes = new mutable.Queue[Seq[N]]()
    storedNodes += List(source)
    val storedNodeSet = new mutable.HashSet[N]()
    storedNodeSet += source

    while (!storedNodes.isEmpty) { // Contains return
      val path = storedNodes.dequeue()
      val n = path.head
      for (n2 ← graph.incidentEdges(n).iterator.flatMap(_.nodes) if !storedNodeSet.contains(n2) && edgeFilter(n, n2)) {
        val np = n2 +: path
        if (n2 == sink) {
          return Some(np)
        }
        storedNodeSet += n2
        storedNodes.enqueue(np)
      }
    }
    None
  }

  def dfsIterator(n: N): Iterator[N] = dfsIterator(n, _ ⇒ true)
  def dfsIterator(n: N, nodeFilter: N ⇒ Boolean): Iterator[N] = simpleIterator(new Graph.ElementOrderer[N] {
    val storedNodes = new mutable.Stack[N]()
    storedNodes.push(n)

    def put(e: N): Unit = if (nodeFilter(e)) storedNodes.push(e)
    def get() = storedNodes.pop()
    def isEmpty: Boolean = storedNodes.isEmpty
  })
  /*
    new AbstractIterator[N] {
    val storedNodes = new mutable.Queue[N]()
    storedNodes += n
    
    def hasNext = !storedNodes.isEmpty
    def next() = {
      val n = storedNodes.dequeue()
      storedNodes ++= graph.incidentEdges(n).flatMap(_.nodes)
      n
    }
  }*/

  override def toString = {
    s"Graph(${nodes.mkString(", ")}; ${edges.mkString(", ")})"
  }

}

object Graph {
  def apply[N <: GraphNode, E <: GraphEdge[N]](): Graph[N, E] = {
    new Graph[N, E](Set(), Map())
  }

  private def nodeMapWith[N, E <: GraphEdge[N]](nodeMap: Map[N, Set[E]], e: E) = {
    nodeMap ++ e.nodes.map(n ⇒ (n, nodeMap.getOrElse(n, Set[E]()) + e))
  }
  private def nodeMapWithout[N, E <: GraphEdge[N]](nodeMap: Map[N, Set[E]], e: E) = {
    nodeMap ++ e.nodes.map(n ⇒ (n, nodeMap.getOrElse(n, Set[E]()) - e))
  }

  trait ElementOrderer[T] {
    def put(e: T): Unit
    def get(): T
    def isEmpty: Boolean
  }
}
