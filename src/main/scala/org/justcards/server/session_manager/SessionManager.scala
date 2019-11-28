package org.justcards.server.session_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import org.justcards.commons._
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.justcards.server.user_manager.Lobby

/**
 * Actor that manages a game session
 */
class SessionManager(lobby: Lobby, gameModel: GameKnowledge) extends Actor {

  import org.justcards.commons.AppError._

  override def receive: Receive = {
    case msg =>
  }
}

/*da ricevere
case class Briscola(seed: String) extends AppMessage
case class Play(card: Card) extends AppMessage
case class TimeoutExceeded(option: String = "") extends AppMessage


da inviare
case class GameStarted(team: TeamId) extends AppMessage
case class Information(handCards: Set[Card], fieldCards: Set[Card]) extends AppMessage
case class ChooseBriscola(option: String = "") extends AppMessage
case class Turn(handCards: Set[Card], fieldCards: Set[Card], timeout: Int) extends AppMessage
case class Played(card: Card) extends AppMessage
case class HandWinner(player: UserId) extends AppMessage
case class MatchWinner(team: TeamId, team1Points: Int, team2Points: Int) extends AppMessage
case class GameWinner(team: TeamId) extends AppMessage
case class OutOfLobby(lobby: LobbyId) extends AppMessage*/

object SessionManager {
  def apply(lobby: Lobby, gameModel: GameKnowledge): Props = Props(classOf[SessionManager], lobby, gameModel)
}