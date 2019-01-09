package api

import java.util.UUID

import model.City

import scala.annotation.tailrec
import scala.util.Random

trait Prob[T] extends Measurable[T] { self ⇒

  def get: T

  def sample(n: Int = 99): Seq[T] = Stream.fill(n)(get)

  def map[U](f: T ⇒ U): Prob[U] = flatMap(f.andThen(Prob.pure))

  def flatMap[U](f: T ⇒ Prob[U]): Prob[U] = Prob(() ⇒ f(get).get)

  def list(n: Int): Prob[Seq[T]] = Prob(() ⇒ sample(n))

  def filter(pred: T ⇒ Boolean = _ => true): Prob[T] = new Prob[T] {

    @tailrec
    override def get: T = {
      val sample = self.get
      if (pred(sample)) sample else get
    }

  }

  def probability(pred: T ⇒ Boolean = _=>true, n: Int = 100000): Double = sample(n).count(pred).toDouble / n

  override def mu(t: T*): Double = t.map(e ⇒ probability(_ ⇒ e == t)).sum

  override def universe: Set[T] = Set.empty

  def density[U](factor: T ⇒ U, n: Int = 99): Map[U, Double] = sample(n).groupBy(factor).map { case (k, it) ⇒ k -> it.size.toDouble / n }

}

object Prob {

  def apply[T](_get: () ⇒ T): Prob[T] = new Prob[T] {
    override def get: T = _get()
  }

  def pure[A](a: A): Prob[A] = Prob(() ⇒ a)

  def shuffle[T](list: List[T]): List[T] = {

    var N = list.length
    var accList = list
    var initList = List.empty[T]

    while (N != 0) {
      val selected = DiscreteUniform(accList).get

      initList = selected :: initList
      accList = accList.filterNot(_ == selected)
      N -= 1

    }

    initList
  }

  implicit class ProbDouble(prob: Prob[Double]) {

    def mean(n: Int = 99): Double = prob.sample(n).sum / n

    def variance(n: Int = 99): Double = prob.sample(n).map(x ⇒ math.pow(x - mean(n), 2)).sum / n

    def std(n: Int = 99): Double = math.sqrt(variance(n))

    def median(n: Int = 99): Double = {

      def appF[T, U](xs: Seq[T ⇒ U], arg: T): Seq[U] = xs.map(_(arg))

      val sortedSample: Seq[Double] = prob.sample(n).sorted
      if (n % 2 == 0) appF[Seq[Double], Double](Seq(_.apply(n / 2 - 1), _.apply(n / 2 + 1)), sortedSample).sum / 2
      else sortedSample.apply(n / 2)
    }

  }

  final case class DiscreteUniform[T](xs: Seq[T]) extends Prob[T] {
    override def get: T = {
      Uniform(0, 1).map(u ⇒ xs.apply((u * xs.length).toInt)).get
    }
  }

  final case class Cities(cities: List[City]) extends Prob[Seq[City]] {
    override def get: Seq[City] = Prob.shuffle(cities)
  }

  final case class Bernoulli(d: Double) extends Prob[Boolean] {
    override def get: Boolean = if (Uniform(0, 1).get <= d) true else false
  }

  case object Ip extends Prob[String] {
    override def get: String = DiscreteUniform(100 to 255).sample(4).mkString(".")
  }

  final case object Uuid extends Prob[String] {
    override def get: String = UUID.randomUUID().toString
  }

  final case class Normal(mean: Double, std: Double) extends Prob[Double] {
    override def get: Double = mean + std * Random.nextGaussian()
  }

  final case class Uniform(min: Double, max: Double) extends Prob[Double] {
    override def get: Double = min + (max - min) * Random.nextDouble()
  }

  final case class Exp(lambda: Double) extends Prob[Double] {
    require(lambda >= 0, s"lambda must be positive. Actual $lambda")
    override def get: Double = {
      val u = Uniform(0, 1).get
      -math.log(u) / lambda
    }
  }

  final case class Poisson(lambda: Double) extends Prob[Int] {
    override def get: Int = {

      val u = Uniform(0, 1)
      val L = math.exp(-lambda)
      var k = 0
      var p = 1d

      if (p <= L) 0
      else {
        do {
          p *= u.get
          k += 1
        } while (p > L)
        k - 1
      }

    }
  }
}