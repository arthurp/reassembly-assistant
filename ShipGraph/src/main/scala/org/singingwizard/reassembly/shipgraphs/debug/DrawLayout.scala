package org.singingwizard.reassembly.shipgraphs.debug

import java.awt.BasicStroke
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.GridLayout
import java.awt.geom.AffineTransform
import java.awt.geom.Ellipse2D
import java.awt.geom.Line2D

import org.singingwizard.reassembly.shipgraphs._
import org.singingwizard.swmath._

import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.SwingUtilities

class LayoutDisplayPanel(layout: Layout) extends JPanel {
  setPreferredSize(new Dimension(600, 600))

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    drawLayout(layout)(g.create().asInstanceOf[Graphics2D])
  }

  def drawLayout(layout: Layout)(implicit g: Graphics2D) = {
    if (layout.hasOverlaps)
      g.drawString("OVERLAPPING", 0, 20)
    g.scale(100, 100)
    g.translate(1.5, 1.5)
    g.setStroke(new BasicStroke(0.02f))
    g.draw(new Ellipse2D.Double(-0.05, -0.05, 0.1, 0.1))
    for (p ← layout.shapes)
      drawPlacedShape(p)
  }

  def drawPlacedShape(shape: PlacedShape)(implicit g: Graphics2D) = {
    import DrawLayout._
    val saveAT = g.getTransform()
    //println(s"Transform:\n${shape.transform}")
    g.transform(shape.transform)
    drawShape(shape.shape)
    g.setTransform(saveAT)
  }

  def drawShape(shape: Shape)(implicit g: Graphics2D) = {
    for ((v1, v2) ← shape.lines) {
      drawLine(v1, v2)
    }
    for (PortPlacement(pos, dir) ← shape.ports) {
      drawLine(pos, pos - (dir*0.1))
    }
  }

  def drawLine(v1: Vec2, v2: Vec2)(implicit g: Graphics2D) = {
    g.draw(new Line2D.Double(v1.x, v1.y, v2.x, v2.y))
  }

  override def update(g: Graphics) {
    paintComponent(g);
  }
}

object DrawLayout {
  def show(layout: Layout): Unit = {
    SwingUtilities.invokeLater(new Runnable {
      def run() = {
        val frame = new JFrame()
        frame.getContentPane().setLayout(new GridLayout(1, 1))
        frame.add(new LayoutDisplayPanel(layout))
        frame.pack()
        frame.setVisible(true)
      }
    })
  }

  import scala.language.implicitConversions
  implicit def mat3ToAffine(t: Mat3): AffineTransform = {
    new AffineTransform(t(0, 0), t(0, 1), t(1, 0), t(1, 1), t(2, 0), t(2, 1))
  }
}