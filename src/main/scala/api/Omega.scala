package api

import model._

trait Omega[T] {
  def universe: Set[T]
}

object Omega {

  def apply[T](u: ⇒ Set[T]): Omega[T] = new Omega[T] {
    override def universe: Set[T] = u
  }

  implicit val omgCoin: Omega[Coin] = Omega(Set(Head, Tail))
}