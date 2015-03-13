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
import javax.swing.Timer
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.geom.Path2D
import org.singingwizard.SlidingPairsWrapping._
import java.awt.Color

class LayoutDisplayPanel(private var glayout_ : Ship) extends JPanel {
  setPreferredSize(new Dimension(600, 600))

  def glayout = glayout_
  def glayout_=(l: Ship) = glayout_ = l; repaint()

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    drawLayout(glayout)(g.create().asInstanceOf[Graphics2D])
  }

  def drawLayout(layout: Ship)(implicit g: Graphics2D) = {
    g.translate(500, 500)
    g.scale(10, 10)
    g.setStroke(new BasicStroke(0.02f))
    for (p ← layout.pieces)
      drawPlacedShape(p)
    g.draw(new Ellipse2D.Double(-0.05, -0.05, 0.1, 0.1))
  }

  def drawPlacedShape(shape: PlacedPiece)(implicit g: Graphics2D) = {
    import DrawLayout._
    val saveAT = g.getTransform()
    //println(s"Transform:\n${shape.transform}")
    g.transform(shape.t)
    drawShape(shape.shape)
    g.setTransform(saveAT)
  }

  def drawShape(shape: Shape)(implicit g: Graphics2D) = {
    val poly = new Path2D.Double()
    poly.moveTo(shape.vertices.last.x, shape.vertices.last.y)
    for (v ← shape.vertices) {
      poly.lineTo(v.x, v.y)
    }    
    val oldC = g.getColor()
    g.setColor(Color.white)
    g.fill(poly)
    g.setColor(oldC)
    g.draw(poly)
    for (PortPlacement(pos, dir) ← shape.ports) {
      drawLine(pos, pos - (dir * 0.1))
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
  def show(layout: Ship): Unit = {
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

  def showMany(makeLayout: () ⇒ Ship): Unit = {
    SwingUtilities.invokeLater(new Runnable {
      def run() = {
        val frame = new JFrame()
        frame.getContentPane().setLayout(new GridLayout(1, 1))
        val display = new LayoutDisplayPanel(makeLayout())
        frame.add(display)
        frame.pack()
        frame.setVisible(true)

        val timer = new Timer(10, new ActionListener() {
          def actionPerformed(e: ActionEvent) = {
            display.glayout = makeLayout()
            frame.repaint()
          }
        })
        timer.start()
      }
    })
  }

  import scala.language.implicitConversions
  implicit def mat3ToAffine(t: Mat3): AffineTransform = {
    new AffineTransform(t(0, 0), t(0, 1), t(1, 0), t(1, 1), t(2, 0), t(2, 1))
  }
}
