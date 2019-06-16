package api

import java.util.UUID

import scala.annotation.tailrec
import scala.util.Random

trait Prob[T] extends Measurable[T] { self ⇒

  import Prob.unit

  def get: T

  def sample(n: Int): Stream[T] = Stream.fill(n)(self.get)

  def probability(predicat: T ⇒ Boolean = (_: T) ⇒ true, n: Int = 10000): Double =
    sample(n).count(predicat).toDouble / n

  override def mu(t: T*): Double = t.map(e ⇒ probability(_ == e)).sum

  // functor
  def map[U](f: T ⇒ U): Prob[U] = Prob(() ⇒ f(get))

  // applicative (+pure)
  def ap[U](fp: Prob[T ⇒ U]): Prob[U] = fp map (_(get))

  // monad (+pure)
  def flatMap[U](f: T ⇒ Prob[U]): Prob[U] = Prob(() ⇒ f(get).get)

  def list(n: Int): Prob[Seq[T]] = Prob(() ⇒ sample(n))

  def filter(pred: T ⇒ Boolean = _ ⇒ true): Prob[T] = new Prob[T] {

    @tailrec
    override def get: T = {
      val sample = self.get
      if (pred(sample)) sample else get
    }

  }

  def density[U](factor: T ⇒ U, n: Int = 99): Map[U, Double] = sample(n).groupBy(factor).map { case (k, it) ⇒ k -> it.size.toDouble / n }

}

object Prob {

  def probTotal[A, B](pA: Prob[A])(f: A ⇒ Prob[B]): Prob[B] = pA flatMap f

  def flatten[T](p: Prob[Prob[T]]): Prob[T] = p.flatMap(identity)

  def product[A, B](pA: Prob[A], pB: Prob[B]): Prob[(A, B)] = pA.ap(pB.map(b ⇒ (a: A) ⇒ (a, b)))

  def map2[A, B, C](pA: Prob[A], pB: Prob[B])(f: (A, B) ⇒ C): Prob[C] = product(pA, pB) map { case (a, b) ⇒ f(a, b) }

  def accept[T](prior: Prob[T], checker: T ⇒ Boolean): T = {

    var accept = false
    var priorSample = prior.get

    while (!accept) {
      priorSample = prior.get
      accept = checker(priorSample)
    }
    priorSample
  }

  def apply[T](_get: () ⇒ T): Prob[T] = new Prob[T] {
    override def get: T = _get()
  }

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
    def to[T](succ: T, echec: T): Prob[T] = this.map(b ⇒ if (b) succ else echec)
  }

  final case class Binomial(n: Int, d: Double) extends Prob[Int] {
    override def get: Int = Bernoulli(d) sample (n) count (_ == true)
  }

  case object Ip extends Prob[String] {
    override def get: String = DiscreteUniform(100 to 255).sample(4).mkString(".")
  }

  final case object Uuid extends Prob[String] {
    override def get: String = UUID.randomUUID().toString
  }

  final case class Normal(mean: Double, std: Double) extends Prob[Double] {

    override def get: Double = {

      val n01 = (
        for {
          u1 ← Uniform(0, 1)
          u2 ← Uniform(0, 1)
        } yield math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.Pi * u2)
      ).get

      mean + std * n01
    }
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