package model

import api.Prob
import api.Prob.DiscreteUniform
import org.scalatest.FunSuite

class DiceTest extends FunSuite {

  test("testIsPair") {

    assert(Dice.isPair(Dice(2)))
    assert(!Dice.isPair(Dice(3)))
  }

  test("testProbabilityPair") {

    val probDice: Prob[Dice] = new Prob[Dice] {
      override def get: Dice = DiscreteUniform(1 to 6).map(Dice(_)).get
    }

    val probPair: Double = Dice.probabilityPair(probDice)
    assert(probPair > 0.47 && probPair < 0.53)
  }

}
