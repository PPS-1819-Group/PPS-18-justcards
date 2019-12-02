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
          startMatch(gameBoard, firstPlayer)
        case BriscolaSetting.SYSTEM =>
          val briscolaSeed = if (gameBoard.getLastCardDeck.isDefined) {
            gameBoard.getLastCardDeck.get.seed
          } else {
            gameBoard.getHandCardsOf(firstPlayer).head.seed
          }
          gameKnowledge setBriscola briscolaSeed
          this broadcast CorrectBriscola(briscolaSeed)
          startMatch(gameBoard,firstPlayer)
        case BriscolaSetting.USER =>
          for (player <- allPlayers)
            sendGameBoardInformation(gameBoard, player)
          context become chooseBriscolaPhase(gameBoard, firstPlayer)
          firstPlayer.userRef ! ChooseBriscola(TIMEOUT)
          timers startSingleTimer(briscolaTimer, Timeout, TIMEOUT seconds)
      }
    case StartMatch(_) => //error

  }

  private def chooseBriscolaPhase(gameBoard: GameBoard, firstPlayer: UserInfo): Receive = {
    case Briscola(seed) if sender() == firstPlayer.userRef =>
      if (gameKnowledge setBriscola seed) {
        timers.cancel(briscolaTimer)
        this broadcast CorrectBriscola(seed)
        startMatch(gameBoard, firstPlayer)
      } else {
        firstPlayer.userRef ! ErrorOccurred(BRISCOLA_NOT_VALID)
      }
    case Timeout =>
      val briscolaSeed = if (gameBoard.getHandCardsOf(firstPlayer).nonEmpty)
        gameBoard.getHandCardsOf(firstPlayer).head.seed
      else
        gameBoard.getLastCardDeck.get.seed
      this broadcast CorrectBriscola(briscolaSeed)
      startMatch(gameBoard, firstPlayer)
  }

  private def inMatch(gameBoard: GameBoard): Receive = {
    case Play(card) if playable(gameBoard, card) && sender() == gameBoard.getTurnPlayer.get.userRef =>
      timers cancel playCardTimer
      var newGameBoard: GameBoard = gameBoard playerPlays card
      if (newGameBoard.getTurnPlayer.isEmpty) {
        newGameBoard = newGameBoard handWinner gameKnowledge.handWinner(newGameBoard.fieldCards)
        newGameBoard = newGameBoard.draw match {
          case None => newGameBoard
          case Some(x) => x
        }
      }
      if (newGameBoard.getHandCardsOfTurnPlayer.get.isEmpty) {
        ???
      } else {
        context become inMatch(newGameBoard)
        turn(newGameBoard, newGameBoard.getTurnPlayer.get)
      }
    case Timeout() =>
      val playableCards = for (card <- gameBoard.getHandCardsOfTurnPlayer.get if playable(gameBoard, card)) yield card
      self ! Play(playableCards.head)
  }

  private def startMatch(gameBoard: GameBoard, firstPlayer: UserInfo): Unit = {
    context become inMatch(gameBoard)
    turn(gameBoard, firstPlayer)
  }

  private def turn(gameBoard: GameBoard, turnPlayer: UserInfo): Unit = {
    for (player <- allPlayers filter (_!=turnPlayer))
      sendGameBoardInformation(gameBoard, player)
    turnPlayer.userRef ! Turn(gameBoard getHandCardsOf turnPlayer, gameBoard.fieldCards.map(_._1), TIMEOUT)
    timers startSingleTimer(playCardTimer, Timeout, TIMEOUT seconds)
  }

  private def playable(gameBoard: GameBoard, card: Card): Boolean =
    gameBoard.getHandCardsOfTurnPlayer.get.contains(card) &&
      gameKnowledge.play(card, gameBoard.fieldCards.map(_._1), gameBoard.getHandCardsOfTurnPlayer.get).isDefined

  private def sendGameBoardInformation(gameBoard: GameBoard, player: UserInfo): Unit =
    player.userRef ! Information(gameBoard getHandCardsOf player, gameBoard.fieldCards.map(_._1))

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
  def apply(lobby: Lobby, gameModel: GameKnowledge): Props = {
    val members = lobby.members.toList.splitAt(lobby.members.size/2)
    Props(classOf[SessionManager], gameModel, Map((Team.TEAM_1, TeamPoints(members._1, 0)), (Team.TEAM_2, TeamPoints(members._2, 0))))
  }
}


private[session_manager] object SessionManagerMessage {

  sealed trait SessionManagerMessage

  case class StartMatch(firstPlayer: UserInfo) extends SessionManagerMessage
  case class Timeout() extends SessionManagerMessage


}