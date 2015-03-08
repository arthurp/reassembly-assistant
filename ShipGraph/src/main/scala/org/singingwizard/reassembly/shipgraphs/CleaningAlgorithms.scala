package org.singingwizard.reassembly.shipgraphs

import org.singingwizard.swmath.Random
import scalaz._
import scala.collection.mutable

object CleaningAlgorithms {
  def removeOverlappingUntilNone(g: Graph, l: Layout): Layout = {
    val overlapping = l.findOverlapping()

    if (overlapping.isEmpty)
      l
    else {
      //val components = g.connectedComponents()
      //val componentMap = components.flatMap(comp ⇒ comp.map(n ⇒ n.id -> comp)).toMap
      val edgesPerNodeID = g.nodes.values.map(n ⇒ n.id -> n.ports.count(g.connectedEdge(_).isDefined)).toMap
      val toRemove = overlapping.minBy(p ⇒ edgesPerNodeID(p.id))
      println(s"Removing: $toRemove (${edgesPerNodeID(toRemove.id)})")
      removeOverlappingUntilNone(g, l.removeShape(toRemove))
    }
  }

  def randomlyConnectComponents(g: Graph): Graph = {
    val anchor +: components = g.connectedComponents().toSeq
    val anchorPorts = mutable.Set() ++ anchor.flatMap(_.ports.filter(g.connectedEdge(_).isEmpty))
    //val componentMap = components.flatMap(comp ⇒ comp.map(n ⇒ n.id -> comp)).toMap
    import Graph._
    val cmds = for (c ← components) yield {
      val cPorts = c.flatMap(_.ports.filter(g.connectedEdge(_).isEmpty))
      (Random.uniformElement(cPorts), Random.uniformElement(anchorPorts)) match {
        case (Some(porta), Some(portb)) ⇒
          anchorPorts -= portb
          anchorPorts ++= cPorts - porta
          connectPorts(porta, portb)
        case _ ⇒
          State.get[Graph]
      }
    }
    cmds.foldLeft(g)((g, cmd) ⇒ cmd exec g)
  }

  def clean(g: Graph): Graph = {
    val newg = randomlyConnectComponents(
      GraphLayoutLens.lensGraphLayout.mod({ l ⇒
        CleaningAlgorithms.removeOverlappingUntilNone(g, l).
          removeImpossibleEdges().
          addMissingEdges()
      }, g)
    )
    if (g == newg) {
      g
    } else {
      clean(newg)
    }
  }
}