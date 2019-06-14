package main

import api.Prob
import api.Prob._
import model.{ Coin, Head, Tail }

object MainBiais extends App {

  // 0.4
  val dataA = List(Head, Tail, Head, Tail, Tail)

  // 0.3
  val dataB = List(Head, Tail, Head, Tail, Tail, Tail, Head, Tail, Tail, Tail)

  val prior = Uniform(0, 1)

  def check(data: Seq[Coin])(p: Double): Boolean = data == Bernoulli(p).to(Head, Tail).sample(data.size).toList

  val simul_p_A: Seq[Double] = (0 to 5000).map { _ ⇒ accept(prior, (d: Double) ⇒ check(dataA)(d)) }
  val simul_p_B: Seq[Double] = (0 to 5000).map { _ ⇒ accept(prior, (d: Double) ⇒ check(dataB)(d)) }

  val prob_p_A = DiscreteUniform(simul_p_A)
  val prob_p_B = DiscreteUniform(simul_p_B)

  import com.quantifind.charts.Highcharts._
  histogram(simul_p_A)
  histogram(simul_p_B)

  val product = Prob.product(prob_p_A, prob_p_B)

  println("P(A > B) = " + product.probability({ case (a, b) ⇒ a >= b }))
  // P(A > B) = 0.66438
}

