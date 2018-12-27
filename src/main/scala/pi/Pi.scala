package pi

import api.Prob
import api.Prob.Uniform

case class Point(x: Double, y: Double)

/**
 * 3,141592653589793
 */
object Pi {

  def generate: Double = {

    val point: Prob[Point] = for {
      x ← Uniform(0, 1)
      y ← Uniform(0, 1)
    } yield Point(x, y)

    val bool: Prob[Boolean] = point.map {
      case Point(x, y) if math.pow(x - 0.5, 2) + math.pow(y - 0.5, 2) <= math.pow(0.5, 2) ⇒ true
      case _ ⇒ false
    }

    val squareArea = 1F
    val rayon = 0.5F

    val N = 1000000

    val probInCircle = bool.prob(identity, N).toFloat
    val circleArea = probInCircle * squareArea

    circleArea / math.pow(rayon, 2)

  }

  def main(args: Array[String]): Unit = {

    (1 to 5).foreach(_ ⇒ println(generate))
  }

}
