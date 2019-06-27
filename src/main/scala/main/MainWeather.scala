package main

import api.Prob
import api.Prob._
import model._

object MainWeather extends App {

  val monthlyMeanTemp: Map[Month, Double] = Map(
    Jannuary -> 5.9,
    February -> 6.8,
    March -> 10.1,
    April -> 12.7,
    May -> 16,
    June -> 19.8,
    July -> 22.3,
    August -> 22,
    September -> 19.2,
    October -> 14.6,
    November -> 10.2,
    December -> 6.7
  )

  val minMaxTemp = Map(
    Jannuary -> (1.4, 10.4),
    February -> (6.8, 11.8),
    March -> (5.2, 15),
    April -> (7.7, 17.7),
    May -> (11, 21),
    June -> (14.2, 25.5),
    July -> (16.3, 28.4),
    August -> (16.3, 27.7),
    September -> (14, 24.5),
    October -> (9.7, 19.6),
    November -> (5.7, 14.7),
    December -> (2.6, 10.9)
  )

  //  val monthlyPrecip = Map(
  //    'Janvier -> 38,
  //    'Fevrier -> 29,
  //    'Mars -> 39,
  //    'Avril -> 25,
  //    'Mai -> 24,
  //    'Juin -> 22,
  //    'Juillet -> 11,
  //    'Aout -> 22,
  //    'Septembre -> 36,
  //    'Octobre -> 52,
  //    'Novembre -> 32,
  //    Decembre -> 36
  //  )

  def weather(
    mean:        Map[Climate, Int],
    std:         Map[Climate, Int],
    probClimate: Prob[Climate]
  ): Prob[(Climate, Double)] = {

    def probTemperature(climate: Climate) = Normal(
      mean(climate),
      std(climate)
    )

    for {
      c ← probClimate
      t ← probTemperature(c)
    } yield c -> t

  }

  val meanMtp = Map(
    Cloudy -> 20,
    Sunny -> 26
  )

  val stdMtp = Map(
    Cloudy -> 2,
    Sunny -> 3
  )

  val probCloudy = Bernoulli(0.16) to (Cloudy, Sunny)

  val weatherMontpellier: Prob[(Climate, Double)] = weather(meanMtp, stdMtp, probCloudy)

  /*
(Sunny,25.922122947490735)
(Cloudy,24.688600650686773)
(Cloudy,19.044043689961402)
(Sunny,22.492105538177498)
(Sunny,28.34775511344805)
(Sunny,23.094036087090053)
(Cloudy,21.517907891128125)
(Sunny,24.773762096303276)
(Sunny,30.17663571438948)
(Sunny,26.261115720392127)
   */

  weatherMontpellier.sample(10).foreach(println)
  println(weatherMontpellier.probability({ case (cl, tmp) ⇒ cl == Cloudy && tmp >= 20 }, 1000))
  // 0.073

}