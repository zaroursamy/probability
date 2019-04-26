package modelization

import api.Prob

object FairCoin extends App {

  val isUnbiaisedPrior = 0.9

  val isUnbiaisedCoin: Prob[Boolean] = Prob.Bernoulli(isUnbiaisedPrior)

  val weight: Prob[String] = for {
    unbiaised ← isUnbiaisedCoin
    flip ← Prob.Bernoulli(if (unbiaised) 0.5 else 0.9)
  } yield if (flip) "H" else "T"

  (0 until 15).foreach { _ ⇒
    println(weight.density(identity, 500).toList)
  }




  println("Mean:")
  (0 until 100).map{_ =>
    weight.density(identity, 500).toList
  }.reduce{ (l1, l2) =>

    List(("H", (l1.head._2+l2.head._2)/2), ("T", (l1.last._2+l2.last._2)/2))
  }.foreach(println)
}
