package org.singingwizard

import scala.language.implicitConversions

package object swmath {
  type Real = Double

  implicit class ExtendReal(val v: Real) extends AnyVal {
    @inline def =~(w: Real) = (v - w).abs <= Epsilons.COMPARE_EPSILON
    @inline def ~^(w: Real) = math.pow(v, w)
    def toString(len: Int) = s"% .${len}f".format(v)
  }


  def clamp(v: Real): Real = if (v < 0) 0 else if (v > 1) 1 else v
  
  def checkNormalized(v: => Vec3) {
    assert(v.length2 =~ 1)
  }
  
  def cosToSin(c: Real) = math.sqrt(1 - c*c)
}