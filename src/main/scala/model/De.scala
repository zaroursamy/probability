package model

case class De(i: Int) {
  require(i >= 0 && i <= 6)
}
