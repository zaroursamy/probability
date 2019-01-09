package api

import model.{ Coin, Head, Tail }
import Omega._

trait Measurable[T] extends Omega[T] {
  val isNonEmpty: Boolean = universe.nonEmpty
  def complementaire(a: Set[T]): Set[T] = universe -- a
  def union(a: Set[T], b: Set[T]): Set[T] = a ++ b
  def mu(t: T*): Double
}

object Measurable {

  def apply[T](measure: T ⇒ Double)(implicit omg: Omega[T]): Measurable[T] = new Measurable[T] {
    override def mu(t: T*): Double = t.distinct.map(measure).sum
    override def universe: Set[T] = omg.universe
  }

  val muCoin: Measurable[Coin] = Measurable[Coin] {
    case Head ⇒ 2
    case Tail ⇒ 1
  }
}
