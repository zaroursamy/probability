package api

import java.util.UUID

import scala.annotation.tailrec
import scala.util.Random

trait Prob[T] extends Measurable[T] { self ⇒

  import Prob.sample

  def get: T

  def map[U](f: T ⇒ U): Prob[U] = flatMap(f.andThen(Prob.unit))

  def flatMap[U](f: T ⇒ Prob[U]): Prob[U] = Prob(() ⇒ f(get).get)

  def list(n: Int): Prob[Seq[T]] = Prob(() ⇒ sample(n)(self))

  def filter(pred: T ⇒ Boolean = _ ⇒ true): Prob[T] = new Prob[T] {

    @tailrec
    override def get: T = {
      val sample = self.get
      if (pred(sample)) sample else get
    }

  }

  def probability(pred: T ⇒ Boolean = _ ⇒ true, n: Int = 100000): Double = sample(n)(self).count(pred).toDouble / n

  override def mu(t: T*): Double = t.map(e ⇒ probability(_ == e)).sum

  def density[U](factor: T ⇒ U, n: Int = 99): Map[U, Double] = sample(n)(self).groupBy(factor).map { case (k, it) ⇒ k -> it.size.toDouble / n }

}

object Prob {

  def apply[T](_get: () ⇒ T): Prob[T] = new Prob[T] {
    override def get: T = _get()
  }

  def apply[T](t: T): Prob[T] = unit(t)

  def sample[T](n: Int)(implicit prob: Prob[T]): Stream[T] = Stream.fill(n)(prob.get)

  def unit[A](a: A): Prob[A] = Prob(() ⇒ a)

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

  final case class DiscreteUniform[T](xs: Seq[T]) extends Prob[T] {
    override def get: T = Uniform(0, 1).map(u ⇒ xs.apply((u * xs.length).toInt)).get
  }

  final case class Cities[C](cities: List[C]) extends Prob[Seq[C]] {
    override def get: Seq[C] = Prob.shuffle(cities)
  }

  final case class Bernoulli(d: Double) extends Prob[Boolean] {
    override def get: Boolean = Uniform(0, 1).get <= d
    def mapp[T](succ: T, echec: T): Prob[T] = this.map(b ⇒ if (b) succ else echec)
  }

  final case class Binomial(n: Int, d: Double) extends Prob[Int] {
    override def get: Int = sample(n)(Bernoulli(d)).count(_ == true)
  }

  case object Ip extends Prob[String] {
    override def get: String = sample(4)(DiscreteUniform(100 to 255)).mkString(".")
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