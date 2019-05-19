package main

import api.Prob

object MainBiais extends App {

  val data = List(false, false, true, false, false)
  val N = data.size

  val prior = Prob.Uniform(0, 1)
  def check(p: Double): Boolean = data == Prob.sample(N)(Prob.Bernoulli(p)).toList

  def accept[T](guesser: Prob[T], checker: T ⇒ Boolean): T = {

    var accept = false
    var guess = guesser.get

    while (!accept) {
      guess = guesser.get
      accept = checker(guess)
    }
    guess
  }

  import com.quantifind.charts.Highcharts._
  histogram((0 to 10000).map { _ ⇒ accept(prior, check) })
}
