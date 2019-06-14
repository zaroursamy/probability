package main

import api.Prob
import api.Prob._

object MainWeather extends App {

  val monthlyMeanTemp: Map[Symbol, Double] = Map(
    'Janvier -> 5.9,
    'Fevrier -> 6.8,
    'Mars -> 10.1,
    'Avril -> 12.7,
    'Mai -> 16,
    'Juin -> 19.8,
    'Juillet -> 22.3,
    'Aout -> 22,
    'Septembre -> 19.2,
    'Octobre -> 14.6,
    'Novembre -> 10.2,
    'Decembre -> 6.7
  )

  val minMaxTemp = Map(
    'Janvier -> (1.4, 10.4),
    'Fevrier -> (6.8, 11.8),
    'Mars -> (5.2, 15),
    'Avril -> (7.7, 17.7),
    'Mai -> (11, 21),
    'Juin -> (14.2, 25.5),
    'Juillet -> (16.3, 28.4),
    'Aout -> (16.3, 27.7),
    'Septembre -> (14, 24.5),
    'Octobre -> (9.7, 19.6),
    'Novembre -> (5.7, 14.7),
    'Decembre -> (2.6, 10.9)
  )

  val monthlyPrecip = Map(
    'Janvier -> 38,
    'Fevrier -> 29,
    'Mars -> 39,
    'Avril -> 25,
    'Mai -> 24,
    'Juin -> 22,
    'Juillet -> 11,
    'Aout -> 22,
    'Septembre -> 36,
    'Octobre -> 52,
    'Novembre -> 32,
    'Decembre -> 36
  )

  def weather(meanTemp: Map[Symbol, Int], stdTemp: Map[Symbol, Int], probCloudy: Prob[Symbol]): Prob[(Symbol, Double)] = {

    def probTemperature(cloudy: Symbol) = Normal(
      meanTemp(cloudy),
      stdTemp(cloudy)
    )

    for {
      c ← probCloudy
      t ← probTemperature(c)
    } yield c -> t

  }

  val meanMtp = Map(
    'cloudy -> 20,
    'sunny -> 26
  )

  val stdMtp = Map(
    'cloudy -> 2,
    'sunny -> 3
  )

  val probCloudy = Bernoulli(0.16) to ('cloudy, 'sunny)

  val weatherMontpellier = weather(meanMtp, stdMtp, probCloudy)

  weatherMontpellier.sample(10).foreach(println)
  println(weatherMontpellier.probability({ case (cl, tmp) ⇒ cl == 'cloudy && tmp >= 20 }, 30))
  // 0.03333333333333333

}