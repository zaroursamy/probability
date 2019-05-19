package main

import api.Prob
import api.Prob._

object MainWeather extends App {

  def weather(): Prob[(Symbol, Long)] = {

    val cloudy = Bernoulli(0.3).mapp('cloudy, 'sunny)

    val meanTemp = Map(
      'cloudy -> 29,
      'sunny -> 35
    )

    val stdTemp = Map(
      'cloudy -> 2,
      'sunny -> 3
    )

    def temp(cloud: Symbol) = Normal(
      meanTemp(cloud),
      stdTemp(cloud)
    )

    for {
      c ← cloudy
      t ← temp(c)
    } yield c -> math.round(t)

  }

  Prob.sample(10)(weather()).foreach(println)

  println(probability[(Symbol, Long)]({ case (cl, tmp) ⇒ cl == 'cloudy && tmp >= 29 }, 1000)(weather()))

}
