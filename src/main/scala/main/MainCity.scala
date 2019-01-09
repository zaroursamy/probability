package main

import api.Distribution.Cities
import model._
import City.dist

object MainCity extends App {

  val cities: List[City] = Paris :: Marseille :: Lyon :: Bordeaux :: Amsterdam :: Rome :: Nil

  val allDists: Seq[(Seq[City], Double)] = Cities(cities)
    .filter(_.apply(1) == Amsterdam)
    .sample(100)
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
