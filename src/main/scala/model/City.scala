package model

sealed trait City

case object Paris extends City
case object Marseille extends City
case object Lyon extends City
case object Bordeaux extends City
case object Amsterdam extends City
case object Rome extends City

object City {

  def dist(city1: City, city2: City): Double = (city1, city2) match {
    case (Paris, Marseille) | (Marseille, Paris) ⇒ 775
    case (Paris, Amsterdam) | (Amsterdam, Paris) ⇒ 508
    case (Paris, Lyon) | (Lyon, Paris) ⇒ 465
    case (Paris, Bordeaux) | (Bordeaux, Paris) ⇒ 584
    case (Paris, Rome) | (Rome, Paris) ⇒ 1422

    case (Marseille, Lyon) | (Lyon, Marseille) ⇒ 314
    case (Marseille, Amsterdam) | (Amsterdam, Marseille) ⇒ 1235
    case (Marseille, Bordeaux) | (Bordeaux, Marseille) ⇒ 645
    case (Marseille, Rome) | (Rome, Marseille) ⇒ 906

    case (Lyon, Bordeaux) | (Bordeaux, Lyon) ⇒ 556
    case (Lyon, Amsterdam) | (Amsterdam, Lyon) ⇒ 926
    case (Lyon, Rome) | (Rome, Lyon) ⇒ 998

    case (Bordeaux, Amsterdam) | (Amsterdam, Bordeaux) ⇒ 1082
    case (Bordeaux, Rome) | (Rome, Bordeaux) ⇒ 1503

    case (Rome, Amsterdam) | (Amsterdam, Rome) ⇒ 1653

    case _ ⇒ 0
  }

}
