package main

import api.Prob
import Prob._
import model.{ Head, Tail }

object MainBiais extends App {

  val data = List(Tail, Tail, Head, Tail, Tail)
  val N = data.size

  val prior = Uniform(0, 1)
  def check(p: Double): Boolean = data == sample(N)(Bernoulli(p).mapp(Head, Tail)).toList

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
