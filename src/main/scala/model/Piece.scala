package model

sealed trait Piece {
  def isPile: Boolean
  def isFace: Boolean = !isPile
}

case object Pile extends Piece {
  override def isPile: Boolean = true
}
case object Face extends Piece {
  override def isPile: Boolean = false
}