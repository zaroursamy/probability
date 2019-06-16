package pi

import model.{ Point, Circle }
import api.Prob
import api.Prob._
import math.pow

/** 3,141592653589793
  */
object Pi {

  def generate(squareArea: Double = 1, N: Int = 100000, circle: Circle = Circle(0.5, 0.5, 0.5)): Double = {

    val uniform = Uniform(0, 1)

    val probPointInCircle: Prob[Boolean] = map2(uniform, uniform) { case (x, y) ⇒ Point(x, y) in circle }

    val probInCircle = probPointInCircle.probability(identity, N)

    val circleArea = probInCircle * squareArea

    circleArea / pow(circle.rayon, 2)
  }
}
