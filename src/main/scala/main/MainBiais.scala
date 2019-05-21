package main

import api.Prob
import api.Prob._
import model.{ Coin, Head, Tail }

object MainBiais extends App {

  val dataA = List(Tail, Tail, Head, Tail, Tail)
  val NA = dataA.size
  println(s"Freq head dataA = ${dataA.count(_ == Head).toDouble / NA}")

  val dataB = List(Head, Tail, Head, Tail, Tail, Tail)
  val NB = dataB.size
  println(s"Freq head dataB = ${dataB.count(_ == Head).toDouble / NB}")

  val prior = Uniform(0, 1)

  def check(data: Seq[Coin])(p: Double): Boolean = data == Bernoulli(p).mapp(Head, Tail).sample(data.size).toList

  val simul_p_A: Seq[Double] = (0 to 5000).map { _ ⇒ accept(prior, (d: Double) ⇒ check(dataA)(d)) }
  val simul_p_B: Seq[Double] = (0 to 5000).map { _ ⇒ accept(prior, (d: Double) ⇒ check(dataB)(d)) }

  val prob_p_A = Prob(() ⇒ DiscreteUniform(simul_p_A))
  val prob_p_B = Prob(() ⇒ DiscreteUniform(simul_p_B))

  import com.quantifind.charts.Highcharts._
  histogram(simul_p_A)
  histogram(simul_p_B)

}

