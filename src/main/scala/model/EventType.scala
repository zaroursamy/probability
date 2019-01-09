package model

import api.Distribution
import api.Distribution.{ Poisson, Uniform }

sealed trait EventType extends Product with Serializable

final case class Picture(winPrice: Double) extends EventType
final case class Video(winPrice: Double, duration: Long) extends EventType

object EventType {

  /**
   * Generate a type of Event (Video or Picture) from a boolean
   * The boolean parameter represents a Bernouilli random variable observation
   * If true, Picture is generated with 5$ win
   * Else, Video is generated with 8$
   * @param bool
   * @return
   */
  def generateEventType(bool: Boolean): Distribution[EventType] = if (bool) Poisson(5).map(x ⇒ Picture(x.toDouble))
  else {
    for {
      win ← Poisson(8)
      dur ← Uniform(10, 30).map(_.toLong)
    } yield Video(win, dur)
  }

}