package model

sealed trait Coin extends Product with Serializable {
  def isTail: Boolean
  def isHead: Boolean = !isTail
}

case object Tail extends Coin {
  override def isTail: Boolean = true
}
case object Head extends Coin {
  override def isTail: Boolean = false
}