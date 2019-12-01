package org.justcards.server.session_manager

import akka.actor.{Actor, Props, Timers}

import scala.concurrent.duration._
import org.justcards.commons._
import org.justcards.server.Commons.{BriscolaSetting, Team, TeamPoints, UserInfo}
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.justcards.server.session_manager.SessionManagerMessage.{StartMatch, Timeout}
import org.justcards.server.user_manager.Lobby


/**
 * Actor that manages a game session
 */
class SessionManager(val gameKnowledge: GameKnowledge, val teams: Map[Team.Value,TeamPoints]) extends Actor with Timers{
  import Team._
  import org.justcards.commons.AppError._

  private case object briscolaTimer
  private case object playCardTimer


  val TIMEOUT: Int = 40

  override def receive: Receive = preMatchBehaviour

  private def preMatchBehaviour: Receive = {
    case StartMatch(firstPlayer) if allPlayers contains firstPlayer =>
      broadcastTo(TEAM_1) (GameStarted(TEAM_1))
      broadcastTo(TEAM_2) (GameStarted(TEAM_2))
      val gameBoard = GameBoard(gameKnowledge, teams(TEAM_1) players, teams(TEAM_2) players, firstPlayer)
      gameKnowledge hasToChooseBriscola match {
        case BriscolaSetting.NOT_BRISCOLA =>
          context become inMatch(gameBoard)
        case BriscolaSetting.SYSTEM =>
          val briscolaSeed = if (gameBoard.getLastCardDeck.isDefined) {
            gameBoard.getLastCardDeck.get.seed
          } else {
            gameBoard.getHandCardsOf(firstPlayer).head.seed
          }
          gameKnowledge setBriscola briscolaSeed
          this broadcast CorrectBriscola(briscolaSeed)
          context become inMatch(gameBoard)
        case BriscolaSetting.USER =>
          for (player <- allPlayers)
            player.userRef ! Information(gameBoard getHandCardsOf player, gameBoard.fieldCards.map(_._1))
          context become chooseBriscolaPhase(gameBoard, firstPlayer)
          firstPlayer.userRef ! ChooseBriscola(TIMEOUT)
          timers startSingleTimer(briscolaTimer, Timeout, TIMEOUT seconds)
      }
    case StartMatch(_) => //error

  }

  private def chooseBriscolaPhase(gameBoard: GameBoard, userInfo: UserInfo): Receive = {
    case Briscola(seed) if sender() == userInfo.userRef =>
      if (gameKnowledge setBriscola seed) {
        this broadcast CorrectBriscola(seed)
        context become inMatch(gameBoard)
      } else {
        userInfo.userRef ! ErrorOccurred(BRISCOLA_NOT_VALID)
      }
    case Timeout =>
      val briscolaSeed = if (gameBoard.getHandCardsOf(userInfo).nonEmpty)
        gameBoard.getHandCardsOf(userInfo).head.seed
      else
        gameBoard.getLastCardDeck.get.seed
      this broadcast CorrectBriscola(briscolaSeed)
      context become inMatch(gameBoard)
  }

  private def inMatch(gameBoard: GameBoard): Receive = {
    case msg: AppMessage => ???
  }


  private def broadcast(appMessage: AppMessage): Unit = {
    for (player <- allPlayers)
      player.userRef ! appMessage
  }

  private def broadcastTo(team: Team.Value) = (appMessage: AppMessage) => {
    for (player <- teams(team).players)
      player.userRef ! appMessage
  }

  private def allPlayers: List[UserInfo] = teams(TEAM_1).players ++ teams(TEAM_2).players
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


private[session_manager] object SessionManagerMessage {

  sealed trait SessionManagerMessage

  case class StartMatch(firstPlayer: UserInfo) extends SessionManagerMessage
  case class Timeout() extends SessionManagerMessage


}