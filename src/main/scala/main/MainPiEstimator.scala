package main

import api.Distribution.Poisson
import pi.Pi.generate

object MainPiEstimator extends App {

  println(generate)
  val pis = (1 to 10).map(_ ⇒ generate)
  pis.foreach(println)
  println("Mean = " + pis.sum / pis.size)
}
