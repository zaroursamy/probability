package api

import org.scalatest.FunSuite

import scala.util.Random

class ProbTest extends FunSuite {

  test("unit & apply methods"){


    val distWeight: Prob[String] = Prob.Bernoulli(0.4).map{ b => if(b) "skinny" else "obese"}
    def mapFood(weight: String): Prob[String] = if(weight == "skinny") Prob.Bernoulli(0.3).map{ b => if(b) "burger" else "celery"}
    else Prob.Bernoulli(0.9).map{ b => if(b) "burger" else "celery"}


    val distFoodKnowingWeight = for {
      w <- distWeight
      f <- mapFood(w)
    } yield f


    distFoodKnowingWeight.density(identity, 100000).foreach(println)
  }
}
