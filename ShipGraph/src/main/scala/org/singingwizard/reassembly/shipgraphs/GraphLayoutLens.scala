package org.singingwizard.reassembly.shipgraphs

import scala.collection.mutable
import scalaz.Lens
import org.singingwizard.swmath._

object GraphLayoutLens {
  import Graph._

  def layoutGraph(g: Graph): Layout = {
    val placedShapes = mutable.Map[NodeID, PlacedShape]()
    val impossibleEdges = mutable.Set[Edge]()
    def placeConnected(inPort: Option[PortPlacement], connected: Node#Port): Unit = {
      if (!placedShapes.contains(connected.node.id)) {
        val trans = inPort match {
          case Some(p) ⇒ p matchingTransform connected.placement
          case None ⇒ Mat3.nil
        }
        val nn = connected.node.transform(trans)
        val nps = PlacedShape(connected.node.id, trans, connected.node.shape)
        placedShapes += nn.id -> nps
        for {
          outp ← nn.ports.toSet[Node#Port]
          inp ← g.connectedPort(outp)
        } placeConnected(Some(outp.placement), inp)
      } else {
        val thisPortPos = placedShapes(connected.node.id).tshape.ports(connected.id.portID).position
        inPort match {
          case Some(PortPlacement(p, _)) if !(p =~ thisPortPos) ⇒
            g.connectedEdge(connected) map { impossibleEdges += _ }
          case _ ⇒ ()
        }
      }
    }
    placeConnected(None, g.anchor.ports(0))

    // Compute the edges that are implied by the computed layout, but don't appear in the graph
    val missingEdges = for {
      // For each shape
      s +: rest ← placedShapes.values.toSeq.tails
      // For each port on the shape s
      (p1, p1i) ← s.tshape.ports.zipWithIndex
      // For the shapes it could connect with
      s2 ← rest
      // For each port on s2
      (p2, p2i) ← s2.tshape.ports.zipWithIndex
      // if the ports match
      e = Edge(PortID(s.id, p1i), PortID(s2.id, p2i))
      if p1.position =~ p2.position && !g.edges.contains(e)
    } yield e

    new Layout(placedShapes.values.toSeq.sortBy(_.id), g.anchor.id, impossibleEdges.toSet, missingEdges.toSet)
  }

  def graphLayout(orig: Graph, l: Layout): Graph = {
    val includedIDs = l.shapes.map(_.id).toSet
    // Build a new graph directly from the Layout
    // This does not use the addNode and connectPorts functions because this version seems clearer
    new Graph {
      val nodes = (
        (for (s ← l.shapes) yield s.id -> Node(s.id, s.shape)).toMap ++
        orig.nodes.filterKeys(id ⇒ !(includedIDs contains id))
      )
      val anchor = nodes(l.anchorID)
      val edges = (
        l.impossibleEdges ++
        orig.edges.filterNot(e ⇒ includedIDs.exists(e.connects(_))) ++
        (for {
          // For each shape
          s +: rest ← l.shapes.tails
          // For each port on the shape s
          (p1, p1i) ← s.tshape.ports.zipWithIndex
          // For the shapes it could connect with
          s2 ← rest
          // For each port on s2
          (p2, p2i) ← s2.tshape.ports.zipWithIndex
          // if the ports match
          e = Edge(PortID(s.id, p1i), PortID(s2.id, p2i))
          if p1.position =~ p2.position && !l.missingEdges.contains(e)
        } yield e)
      )
    }
  }

  val lensGraphLayout = Lens.lensu[Graph, Layout](graphLayout, layoutGraph)
}