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

    val u: Prob[Double] = Uniform(0, 10)
    val apGenerateModulo: Prob[Int] = u.ap(generateModulo)

    apGenerateModulo.sample(10).foreach(println)
  }

  test("testFilter") {

  }

  test("testFlatMap") {

  }

}
