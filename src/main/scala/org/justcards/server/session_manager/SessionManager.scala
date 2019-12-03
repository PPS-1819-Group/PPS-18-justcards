package org.justcards.server.session_manager

import akka.actor.{Actor, Props, Timers}

import scala.concurrent.duration._
import org.justcards.commons._
import org.justcards.server.Commons.{BriscolaSetting, Team, TeamPoints, UserInfo}
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.justcards.server.session_manager.SessionManagerMessage.{StartMatch, Timeout, endMatchMessage}
import org.justcards.server.user_manager.Lobby


/**
 * Actor that manages a game session
 */
class SessionManager(val gameKnowledge: GameKnowledge, var teams: Map[Team.Value,TeamPoints], lobby: Lobby) extends Actor with Timers{
  import Team._
  import org.justcards.commons.AppError._

  private case object briscolaTimer
  private case object playCardTimer

  self ! StartMatch(teams(TEAM_1).players.head)

  val TIMEOUT: Int = 40

  var firstPlayerMatch: UserInfo = _

  override def receive: Receive = preMatchBehaviour

  private def preMatchBehaviour: Receive = {
    case StartMatch(firstPlayer) if allPlayers contains firstPlayer =>
      firstPlayerMatch = firstPlayer
      broadcastTo(TEAM_1) (GameStarted(TEAM_1))
      broadcastTo(TEAM_2) (GameStarted(TEAM_2))
      val gameBoard = GameBoard(gameKnowledge, teams(TEAM_1) players, teams(TEAM_2) players, firstPlayer)
      for (player <- allPlayers)
        sendGameBoardInformation(gameBoard, player)
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
      sender() ! Played(card)
      timers cancel playCardTimer
      var newGameBoard: GameBoard = gameBoard playerPlays card
      if (newGameBoard.getTurnPlayer.isEmpty) {
        val playerWinner = gameKnowledge.handWinner(newGameBoard.fieldCards)
        this broadcast HandWinner(playerWinner)
        newGameBoard = newGameBoard handWinner playerWinner
        newGameBoard = newGameBoard.draw match {
          case None => newGameBoard
          case Some(x) => x
        }
      }
      if (newGameBoard.getHandCardsOfTurnPlayer.get.isEmpty) {
        context become endMatch(newGameBoard)
        self ! endMatchMessage(newGameBoard.getTurnPlayer.get)
      } else {
        context become inMatch(newGameBoard)
        turn(newGameBoard, newGameBoard.getTurnPlayer.get)
      }
    case Play(_) =>
      sender() ! ErrorOccurred(CARD_NOT_VALID)
    case Timeout() =>
      val playableCards: Set[Card] = for (card <- gameBoard.getHandCardsOfTurnPlayer.get if playable(gameBoard, card)) yield card
      self ! Play(playableCards.head)
  }

  private def endMatch(gameBoard: GameBoard): Receive = {
    case endMatchMessage(lastHandWinner: UserInfo) =>
      var tookCardsTeam: Map[Team.Value, Set[Card]] = teams.keySet.map((_, Set[Card]())).toMap
      for (team <- Team.values) {
        for(player <- teams(team).players) {
          tookCardsTeam = tookCardsTeam + (team -> (tookCardsTeam(team) ++ gameBoard.getTookCardsOf(player)))
        }
      }
      val lastHandWinnerTeam = teams.find(_._2.players contains lastHandWinner).get._1
      gameKnowledge.matchPoints(tookCardsTeam(TEAM_1), tookCardsTeam(TEAM_2), lastHandWinnerTeam) //TODO
      val matchInfo = gameKnowledge.matchWinner(tookCardsTeam(TEAM_1), tookCardsTeam(TEAM_2), lastHandWinnerTeam)
      teams = teams + (TEAM_1 -> TeamPoints(teams(TEAM_1).players, teams(TEAM_1).points + matchInfo._2))
      teams = teams + (TEAM_2 -> TeamPoints(teams(TEAM_2).players, teams(TEAM_2).points + matchInfo._3))
      broadcast(MatchWinner(matchInfo._1, matchInfo._2, matchInfo._3))
      gameKnowledge.sessionWinner(teams(TEAM_1).points, teams(TEAM_2).points) match {
        case None =>
          context become preMatchBehaviour
          self ! StartMatch(gameBoard getPlayerAfter firstPlayerMatch)
        case Some(winner) =>
          this broadcast GameWinner(winner)
          this broadcast OutOfLobby(lobby)
          context stop self
      }

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


object SessionManager {
  def apply(lobby: Lobby, gameModel: GameKnowledge): Props = {
    val members = lobby.members.toList.splitAt(lobby.members.size/2)
    Props(classOf[SessionManager], gameModel, Map((Team.TEAM_1, TeamPoints(members._1, 0)), (Team.TEAM_2, TeamPoints(members._2, 0))), lobby)
  }
}


private[session_manager] object SessionManagerMessage {

  sealed trait SessionManagerMessage

  case class StartMatch(firstPlayer: UserInfo) extends SessionManagerMessage
  case class endMatchMessage(lastHandWinner: UserInfo) extends SessionManagerMessage
  case class Timeout() extends SessionManagerMessage


}