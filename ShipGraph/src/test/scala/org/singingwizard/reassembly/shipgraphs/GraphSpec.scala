package org.singingwizard.reassembly.shipgraphs

import org.specs2._
import org.specs2.runner.JUnitRunner
import org.singingwizard.swmath._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class GraphSpec extends mutable.Specification {
  "A ship graph" >> {
    "Empty has one node" >> {
      Graph.empty.nodes.size must_== 1
    }
    "Adding one node should have 2 nodes" >> {
      val (g, _) = Graph.empty.addNode(Shapes.rightTriangle)
      g.nodes.size must_== 2
    }
    "Nodes should be connected both ways" >> {
      val (g, n) = Graph.empty.addNode(Shapes.rightTriangle)
      val g2 = g.connectPorts(n.ports(0), g.anchor.ports(0))
      g2.nodes.size must_== 2
      g2.edges.size must_== 1
      g2.connectedPort(n.ports(0)) ==== Some(g.anchor.ports(0))
      g2.connectedPort(g.anchor.ports(0)) ==== Some(n.ports(0))
    }
  }
}

@RunWith(classOf[JUnitRunner])
class GraphLayoutSpec extends mutable.Specification {
  "A ship graph can be layed out" >> {
    "Empty lays out at 0,0" >> {
      val l = GraphLayoutLens.layoutGraph(Graph.empty)
      l.shapes.size ==== 1
      l.shapes.head.transform ==== Mat3.nil
    }
    "Connected ports" >> {
      val (g, n) = Graph.empty.addNode(Shapes.rightTriangle)
      val g2 = g.connectPorts(n.ports(0), g.anchor.ports(0))
      val l = GraphLayoutLens.layoutGraph(g2)
      l.shapes.size ==== 2
      l.shapes(0).tshape.ports(0).position ==== l.shapes(1).tshape.ports(0).position
    }
  }
}



