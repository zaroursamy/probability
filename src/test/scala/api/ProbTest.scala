package api

import api.Prob._
import org.scalatest.FunSuite

class ProbTest extends FunSuite {

  test("testMap") {

  }

  test("testMu") {

  }

  test("testList") {

  }

  test("testAp") {

    val generateModulo: Prob[Double => Int] = new Prob[Double => Int] {
      override def get: Double => Int = Bernoulli(0.5).mapp((d: Double) => math.round(d).toInt%2, (d:Double) => math.round(d).toInt%3).get
    }

    val apGenerateModulo: Prob[Int] = Uniform(0,10).ap(generateModulo)

    Prob.sample(10)(apGenerateModulo).foreach(println)
  }

  test("testFilter") {

  }

  test("testFlatMap") {

  }

}
