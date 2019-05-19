package pi

import model.{ Point, Circle }
import api.Prob
import api.Prob._
import math.pow

/**
 * 3,141592653589793
 */
object Pi {

  def generate: Double = {

    val squareArea = 1d
    val N = 1000000
    val rayon, cx, cy = 0.5d

    val circle = Circle(cx, cy, rayon)

    val probPointInCircle = for {
      x ← Uniform(0, 1)
      y ← Uniform(0, 1)
    } yield Point(x, y) in circle

    val probInCircle = probability[Boolean](identity, N)(probPointInCircle)
    val circleArea = probInCircle * squareArea

    circleArea / pow(rayon, 2)
  }
}
