package model

import math.pow

case class Point(x: Double, y: Double) {

  def in(circle: Circle): Boolean = {
    import circle._
    (pow(x - cx, 2) + pow(y - cy, 2)) <= pow(rayon, 2)
  }

}