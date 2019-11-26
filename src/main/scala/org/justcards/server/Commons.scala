package org.justcards.server

import akka.actor.ActorRef

object Commons {

  object BriscolaSetting extends Enumeration {
    type BriscolaSetting = Value
    val USER, SYSTEM, NOT_BRISCOLA = Value
  }

  object Team extends Enumeration {
    type Team = Value
    val TEAM_1: Value = Value(1,"team 1")
    val TEAM_2: Value = Value(2,"team 2")
  }

  case class UserInfo(username: String, userRef: ActorRef)
}