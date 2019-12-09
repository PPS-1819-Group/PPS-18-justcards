package org.justcards.server

import akka.actor.ActorRef
import org.justcards.commons.games_rules.GameRules
import org.justcards.commons.{Card, TeamId, UserId}

object Commons {

  object BriscolaSetting extends Enumeration {
    type BriscolaSetting = Value
    val USER, SYSTEM, NOT_BRISCOLA = Value
  }

  object Team extends Enumeration {
    type Team = Value
    val TEAM_1: Value = Value(1,"team 1")
    val TEAM_2: Value = Value(2,"team 2")

    implicit def toTeamId(team: Team.Value): TeamId = TeamId(team toString)

  }


  case class UserInfo(username: String, userRef: ActorRef)

  implicit def toUserId(userInfo: UserInfo): UserId = UserId(1, userInfo.username)

  case class PlayerCards(hand: Set[Card], took: Set[Card])

  case class TeamPoints(players: List[UserInfo], points: Int)
}
