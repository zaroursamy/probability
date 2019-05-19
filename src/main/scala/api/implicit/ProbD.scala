package api.`implicit`

import api.Prob
import Prob._

object ProbD {
  def appF[T, U](xs: Seq[T ⇒ U], arg: T): Seq[U] = xs.map(_(arg))

  implicit class IProbD(prob: Prob[Double]) {

    def mean(n: Int = 99): Double = sample(n)(prob).sum / n

    def variance(n: Int = 99): Double = sample(n)(prob).map(x ⇒ math.pow(x - mean(n), 2)).sum / n

    def std(n: Int = 99): Double = math.sqrt(variance(n))

    def median(n: Int = 99): Double = {

      val sortedSample: Seq[Double] = sample(n)(prob).sorted
      if (n % 2 == 0) appF[Seq[Double], Double](Seq(_.apply(n / 2 - 1), _.apply(n / 2 + 1)), sortedSample).sum / 2
      else sortedSample.apply(n / 2)
    }

  }
}
