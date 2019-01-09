package model

sealed trait Coin {
  def isPile: Boolean
  def isFace: Boolean = !isPile
}

case object Tail extends Coin {
  override def isPile: Boolean = true
}
case object Head extends Coin {
  override def isPile: Boolean = false
}