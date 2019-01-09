package main

import api.Prob
import api.Prob.Bernoulli

/**
  * Vous êtes directeur de cabinet du ministre de la santé.
  * Une maladie est présente dans la population, dans la proportion d'une personne malade sur 10000.
  * Un responsable d'un grand laboratoire pharmaceutique vient vous vanter son nouveau test de dépistage :
  *   si une personne est malade, le test est positif à 99%.
  *   Si une personne n'est pas malade, le test est positif à 0,1%.
  */
object MainDisease extends App {

  val probMalade = Bernoulli(0.0001)

  def probPositifTest(pMalade: Prob[Boolean]): Prob[Boolean] = pMalade.flatMap{ b =>
    if(b)  Bernoulli(0.99)
    else Bernoulli(0.001)
  }

  // P(malade | positif) = P(positif | malade) * P(malade)/P(positif)

  val pPositif = probPositifTest(probMalade).probability(identity)
  val pMalade = probMalade.probability(identity)
  val pPositif_malade = probPositifTest(probMalade.filter(identity)).probability(identity)

  println(s"P(positif) = $pPositif")
  println(s"P(malade) = $pMalade")
  println(s"P(positif | malade) = $pPositif_malade")
  println(s"P(malade | positif) = ${pPositif_malade*pMalade/pPositif}")


}
