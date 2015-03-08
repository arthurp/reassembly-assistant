package org.singingwizard.reassembly.shipgraphs

import scala.collection.mutable
import scalaz.Lens
import org.singingwizard.swmath._

object GraphLayoutLens {
  import Graph._
  
  def layoutGraph(g: Graph): Layout = {
    val placedShapes = mutable.Map[NodeID, PlacedShape]()
    def placeConnected(inPort: Option[PortPlacement], connected: Node#Port): Unit = {
      if (!placedShapes.contains(connected.node.id)) {
        val trans = inPort match { 
          case Some(p) => p matchingTransform connected.placement
          case None => Mat3.nil
        }
        val nn = connected.node.transform(trans)
        val nps = PlacedShape(connected.node.id, trans, connected.node.shape)
        placedShapes += nn.id -> nps
        for {
          outp <- nn.ports.toSet[Node#Port]
          inp <- g.connectedPort(outp)
        } placeConnected(Some(outp.placement), inp)
      }
    }
    placeConnected(None, g.anchor.ports(0))
    Layout(placedShapes.values.toSeq.sortBy(_.id), g.anchor.id)
  }
  
  def graphLayout(g: Graph, l: Layout): Graph = {
    var g: Graph = new Graph {
      val nodes = (for (s <- l.shapes) yield s.id -> Node(s.id, s.shape)).toMap    
      val anchor = nodes(l.anchorID)
      val edges = Set[Edge]()
    }
    
    for (s +: rest <- l.shapes.tails) {
      for {
        (p1, p1i) <- s.tshape.ports.zipWithIndex
        s2 <- rest
        (p2, p2i) <- s2.tshape.ports.zipWithIndex
        if p1.position =~ p2.position
      } g = g.connectPorts(g.lookup(s.id).ports(p1i), g.lookup(s2.id).ports(p2i))
    }
    g
  }
  
  val lensGraphLayout = Lens.lensu[Graph, Layout] (graphLayout, layoutGraph)
}