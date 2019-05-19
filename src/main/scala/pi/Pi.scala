package pi

import model.{ Point, Circle }
import api.Prob
import api.Prob.Uniform
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

    val probPoint: Prob[Point] = for {
      x ← Uniform(0, 1)
      y ← Uniform(0, 1)
    } yield Point(x, y)

    val probBool: Prob[Boolean] = probPoint map {
      case point: Point if point in circle ⇒ true
      case _                               ⇒ false
    }

    val probInCircle = probBool probability (identity, N)
    val circleArea = probInCircle * squareArea

    circleArea / pow(rayon, 2)
  }
}
