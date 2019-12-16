package org.justcards.server

import java.io.File
import java.nio.file.{Files, StandardCopyOption}

import akka.actor.ActorRef
import org.justcards.commons.games_rules.knowledge.RuleKnowledge.RULES_PATH
import org.justcards.commons.{Card, TeamId, UserId}
import org.justcards.server.knowledge_engine.GamesManager.DefaultGame.DefaultGame
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge.GAMES_PATH

object Commons {

  object Team extends Enumeration {
    type Team = Value
    val TEAM_1: Value = Value(1,"team 1")
    val TEAM_2: Value = Value(2,"team 2")

    implicit def toTeamId(team: Team.Value): TeamId = TeamId(team toString)

  }


  case class UserInfo(username: String, userRef: ActorRef)

  implicit def toUserId(userInfo: UserInfo): UserId = UserId(1, userInfo.username)

  case class PlayerCards(hand: Set[Card], took: Set[Card])

  case class TeamPoints(players: List[String], points: Int)

  private[this] def filesInDirectory(directory: File):List[File] = directory match {
    case d if d.exists && d.isDirectory => d.listFiles.filter(_.isFile).toList
    case _ => List()
  }

  def exportGames(defaultGames: DefaultGame*): Unit = {
    val defaultGamesNames = defaultGames.map(_.toString + ".pl")
    val gamesDirectory = new File(GAMES_PATH)
    gamesDirectory mkdir
    val games = filesInDirectory(gamesDirectory) map(_.getName.toLowerCase)

    defaultGamesNames filter (g => !games.contains(g.toLowerCase)) foreach { f =>
      Files.copy(
        getClass.getResourceAsStream(RULES_PATH + "games/" + f),
        new File(gamesDirectory.getAbsolutePath + "/" + f).toPath
      )
    }
  }

}
