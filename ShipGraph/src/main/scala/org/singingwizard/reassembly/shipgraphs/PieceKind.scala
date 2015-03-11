package org.singingwizard.reassembly.shipgraphs

case class PieceKind(shape: Shape, hitpoints: Int, mass: Int = 0, name: Option[String] = None) {
  def ports = shape.ports
  
  override def toString = name getOrElse s"PieceKind($shape, $hitpoints)"
}

object PieceKinds {
  val core = PieceKind(Shapes.octogon, 100000, 100, Some("core"))
  val squareWeak = PieceKind(Shapes.square, 10, 10, Some("squareWeak"))
  val squareStrong = PieceKind(Shapes.square, 100, 100, Some("squareStrong"))
}