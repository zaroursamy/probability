package main

import api.Prob.Cities
import model._
import City.dist
import api.Prob

object MainCity extends App {

  val cities: List[City] = Paris :: Marseille :: Lyon :: Bordeaux :: Amsterdam :: Rome :: Nil

  val allDists: Seq[(Seq[City], Double)] = Prob
    .sample(100)(Cities(cities).filter(_.apply(1) == Amsterdam))
    .map { aSet ⇒
      aSet
        .foldLeft((Seq.empty[City], 0d, Option.empty[City])) {
          case ((accCities, accDist, _), newCity) ⇒

            (
              accCities :+ newCity,

              accDist + dist(newCity, accCities.lastOption match {
                case Some(c) ⇒ c
                case _       ⇒ newCity
              }),

              Some(newCity) // Pour ne pas traverser encore une fois la liste par la suite
            )
        }
    }
    .map {
      case (accCities, accDist, lastOptCity) ⇒

        val finalDist: Double = (for {
          first ← accCities.headOption
          last ← lastOptCity
        } yield accDist + dist(first, last)).getOrElse(0d)

        (accCities, finalDist)

    }

  allDists.distinct.sortWith(_._2 < _._2).foreach(println)

}
