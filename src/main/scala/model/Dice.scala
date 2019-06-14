package model

import api.Prob

case class Dice(value: Int) {
  require(value > 0 && value <= 6)
}

object Dice {
  def isPair(dice: Dice): Boolean = dice.value % 2 == 0
  def probabilityPair(probDice: Prob[Dice]): Double = probDice.probability(isPair, 1000)
}