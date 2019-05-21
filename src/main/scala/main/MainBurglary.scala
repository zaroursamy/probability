package main

import api.Prob
import api.Prob._

object MainBurglary extends App {

  val pBurglary: Prob[Boolean] = Bernoulli(0.01)

  val pEarthquake: Prob[Boolean] = Bernoulli(0.0001)

  def pAlarm(burglary: Prob[Boolean], earthquake: Prob[Boolean]): Prob[Boolean] = (for {
    b ← burglary
    e ← earthquake
  } yield (b, e) match {
    case (true, true)   ⇒ 0.99
    case (true, false)  ⇒ 0.9
    case (false, false) ⇒ 0.001
    case (false, true)  ⇒ 0.1
  })
    .flatMap(Bernoulli)

  def pSamyCall(alarm: Prob[Boolean]): Prob[Boolean] = alarm.flatMap { b ⇒
    if (b) Bernoulli(0.7)
    else Bernoulli(0.01)
  }

  val N = 100000

  val probBurglary = pBurglary.probability(identity, N)
  println(s"P(burglary = true) = $probBurglary")

  val probSamyCall = pSamyCall(pAlarm(pBurglary, pEarthquake)).probability(identity, N)
  println(s"P(samyCall = true) = $probSamyCall")

  val probSamyCall_burglary = pSamyCall(pAlarm(pBurglary.filter(identity), pEarthquake)).probability(identity, N)
  println(s"P(samyCall = true | burglary = true) = $probSamyCall_burglary")

  println(s"P(burglary = true | samyCall = true) = ${probSamyCall_burglary * probBurglary / probSamyCall}")

}
