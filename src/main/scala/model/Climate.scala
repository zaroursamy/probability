package model

sealed trait Climate extends Product with Serializable
case object Cloudy extends Climate
case object Sunny extends Climate