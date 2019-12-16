package org.justcards.server.session_manager

import akka.actor.{Actor, ActorContext, ActorLogging, ActorRef, Props, Timers}

import scala.concurrent.duration._
import org.justcards.commons._
import org.justcards.server.Commons._
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.justcards.server.user_manager.Lobby
import SessionManager.TeamPoints


/**
 * Actor that manages a game session.
 * @param gameKnowledge the knowledge of the game to play
 * @param teams the teams
 * @param lobby the lobby
 * @param players the players
 */
class SessionManager(gameKnowledge: GameKnowledge, var teams: Map[Team.Value,TeamPoints], lobby: LobbyId, var players: Map[String, UserInfo]) extends Actor with Timers with ActorLogging{
  import Team._
  import org.justcards.commons.AppError._
  import SessionManager._
  import SessionManagerMessage._

  self ! StartMatch(None)

  var firstPlayerMatch: UserInfo = _

  override def receive: Receive = preMatchBehaviour orElse defaultBehaviour

  private def defaultBehaviour: Receive = {
    case LogOutAndExitFromGame(user) if user.userRef == sender() => addFakeUser(user)
  }

  private def preMatchBehaviour: Receive = {
    case StartMatch(firstPlayer) if firstPlayer.isEmpty || (allPlayers contains firstPlayer.get) =>
      val gameBoard = GameBoard(gameKnowledge, teams(TEAM_1) players, teams(TEAM_2) players, firstPlayer)
      firstPlayerMatch = (gameBoard.optionTurnPlayer get) toUserInfo
      val newTeams: List[(UserId, TeamId)] = gameBoard.turn.map(user => (
        UserId(1, user),
        TeamId(teams.filterKeys(teams(_).players.contains(user)).head._1 toString)
      ))
      this broadcast GameStarted(newTeams)
      sendGameBoardInformation(gameBoard, allPlayers)

      gameKnowledge hasToChooseBriscola match {
        case BriscolaSetting.NOT_BRISCOLA =>
          context toMatch(gameBoard, firstPlayerMatch)
        case BriscolaSetting.SYSTEM =>
          val briscola =
            if (gameBoard.optionLastCardDeck.isDefined) gameBoard.optionLastCardDeck.get
            else gameBoard.handCardsOf(firstPlayerMatch).head
          gameKnowledge setBriscola briscola.seed
          this broadcast CorrectBriscola(briscola.seed, Some(briscola.number))
          context toMatch(gameBoard,firstPlayerMatch)
        case BriscolaSetting.USER =>
          context >>> chooseBriscolaPhase(gameBoard, firstPlayerMatch)
          firstPlayerMatch.userRef ! ChooseBriscola(gameKnowledge.seeds, TIMEOUT_TO_USER)
          timers startSingleTimer(TimeoutId, Timeout, TIMEOUT seconds)
      }
  }

  private def chooseBriscolaPhase(gameBoard: GameBoard, firstPlayer: UserInfo): Receive = {
    case Briscola(seed) if sender() == firstPlayer.userRef =>
      if (gameKnowledge setBriscola seed) {
        timers cancel TimeoutId
        this broadcast CorrectBriscola(seed)
        context toMatch(gameBoard, firstPlayer)
      } else {
        firstPlayer.userRef ! ErrorOccurred(BRISCOLA_NOT_VALID)
      }
    case Timeout | TimeoutExceeded(_) =>
      val briscolaSeed = if (gameBoard.handCardsOf(firstPlayer).nonEmpty)
        gameBoard.handCardsOf(firstPlayer).head.seed
      else
        gameBoard.optionLastCardDeck.get.seed
      this broadcast CorrectBriscola(briscolaSeed)
      gameKnowledge setBriscola briscolaSeed
      context toMatch(gameBoard, firstPlayer)
    case LogOutAndExitFromGame(user) if sender() == firstPlayer.userRef =>
      addFakeUser(user)
      self ! TimeoutExceeded()
  }

  private def inMatch(gameBoard: GameBoard): Receive = {
    case Play(card) if playable(gameBoard, card) && isCorrectPlayer(gameBoard, sender) =>
      if(sender() != self) sender() ! Played(card)
      else gameBoard.optionTurnPlayer.get.toUserInfo.userRef ! Played(card)
      timers cancel TimeoutId
      val newGameBoard: GameBoard = gameBoard playerPlays card
      if (newGameBoard.optionTurnPlayer.nonEmpty) context toMatch(newGameBoard, newGameBoard.optionTurnPlayer.get.toUserInfo)
      else handleEndHand(newGameBoard)
    case Play(_) =>
      sender() ! ErrorOccurred(CARD_NOT_VALID)
    case Timeout | TimeoutExceeded(_) =>
      val playableCards: Set[Card] = for (card <- gameBoard.handCardsOfTurnPlayer.get if playable(gameBoard, card)) yield card
      self ! Play(playableCards.head)
    case LogOutAndExitFromGame(user) if user.userRef == sender && isCorrectPlayer(gameBoard, sender) =>
      addFakeUser(user)
      self ! TimeoutExceeded()
  }

  private def handleEndHand(gameBoard: GameBoard): Unit = {
    sendGameBoardInformation(gameBoard, allPlayers)
    val playerWinner = gameKnowledge.handWinner(gameBoard.fieldCards.map(elem => (elem._1, elem._2.toUserInfo)))
    this broadcast HandWinner(playerWinner)
    var newGameBoard = gameBoard handWinner playerWinner
    newGameBoard = newGameBoard.draw match {
      case None => newGameBoard
      case Some(x) => x
    }
    context toMatch(newGameBoard, newGameBoard.optionTurnPlayer.get.toUserInfo)
  }

  private def endMatch(gameBoard: GameBoard): Receive = {
    case EndMatchMessage(lastHandWinner: UserInfo) =>
      var tookCardsTeam: Map[Team.Value, Set[Card]] = teams.keySet.map((_, Set[Card]())).toMap
      for (team <- Team.values; player <- teams(team).players) {
        tookCardsTeam = tookCardsTeam + (team -> (tookCardsTeam(team) ++ gameBoard.tookCardsOf(player)))
      }
      val lastHandWinnerTeam = teams.find(_._2.players contains lastHandWinner.username).get._1
      val matchPoints = gameKnowledge.matchPoints(tookCardsTeam(TEAM_1), tookCardsTeam(TEAM_2), lastHandWinnerTeam)
      val pointsForSession = gameKnowledge.matchWinner(tookCardsTeam(TEAM_1), tookCardsTeam(TEAM_2), lastHandWinnerTeam)
      teams = teams + (TEAM_1 -> TeamPoints(teams(TEAM_1).players, teams(TEAM_1).points + pointsForSession._2))
      teams = teams + (TEAM_2 -> TeamPoints(teams(TEAM_2).players, teams(TEAM_2).points + pointsForSession._3))
      broadcast(MatchWinner(pointsForSession._1, (matchPoints._1, matchPoints._2), (teams(TEAM_1).points, teams(TEAM_2).points)))
      gameKnowledge.sessionWinner(teams(TEAM_1).points, teams(TEAM_2).points) match {
        case None =>
          context >>> preMatchBehaviour
          self ! StartMatch((gameBoard playerAfter firstPlayerMatch).map(_.toUserInfo))
        case Some(winner) =>
          this broadcast GameWinner(winner)
          this broadcast OutOfLobby(lobby)
          context stop self
      }

  }


  private def addFakeUser(user: UserInfo): Unit =
    players = players.updated(user.username, UserInfo(user.username, context.system.actorOf(FakeUser())))


  private def turn(gameBoard: GameBoard, turnPlayer: UserInfo): Unit = {
    if (gameBoard.handCardsOfTurnPlayer.get.isEmpty) {
      context >>> endMatch(gameBoard)
      self ! EndMatchMessage(gameBoard.optionTurnPlayer.get.toUserInfo)
    } else {
      sendGameBoardInformation(gameBoard, allPlayers filter (_!=turnPlayer))
      turnPlayer.userRef ! Turn(gameBoard handCardsOf turnPlayer, gameBoard.fieldCards.map(_._1), TIMEOUT_TO_USER)
      timers startSingleTimer(TimeoutId, Timeout, TIMEOUT seconds)
    }
  }

  private def isCorrectPlayer(gameBoard: GameBoard, sender: ActorRef): Boolean =
    sender == gameBoard.optionTurnPlayer.get.toUserInfo.userRef || sender == self

  private def playable(gameBoard: GameBoard, card: Card): Boolean =
      gameKnowledge.play(card, gameBoard.fieldCards.map(_._1), gameBoard.handCardsOfTurnPlayer.get).isDefined

  private def sendGameBoardInformation(gameBoard: GameBoard, receivers: List[UserInfo]): Unit =
    for (player <- receivers)
      player.userRef ! Information(gameBoard handCardsOf player, gameBoard.fieldCards.map(_._1))

  private def broadcast(appMessage: AppMessage): Unit = {
    for (player <- allPlayers)
      player.userRef ! appMessage
  }

  private def allPlayers: List[UserInfo] = players.values.toList

  private implicit class RichContext(context: ActorContext){

    def >>>(behaviour: Receive): Unit = context become behaviour.orElse(defaultBehaviour)

    def toMatch(gameBoard: GameBoard, firstPlayer: UserInfo): Receive = {
      val receive = inMatch(gameBoard)
      context >>> receive
      turn(gameBoard, firstPlayer)
      receive
    }
  }

  private implicit class RichString(player: String){
    def toUserInfo: UserInfo = players(player)
  }

  implicit def userInfoToString(userInfo: UserInfo): String = userInfo.username

}


object SessionManager {

  case class LogOutAndExitFromGame(user: UserInfo)

  private[session_manager] case class TeamPoints(players: List[String], points: Int)

  val TIMEOUT: Int = 40
  val TIMEOUT_TO_USER: Int = 30

  /**
   * Create a new SessionManager
   * @param lobby the lobby
   * @param gameKnowledge the knowledge about the game to play
   * @return a new SessionManager
   */
  def apply(lobby: Lobby, gameKnowledge: GameKnowledge): Props = {
    import Lobby._
    val members = lobby.members.toList.splitAt(lobby.members.size/2)
    Props(classOf[SessionManager],
      gameKnowledge,
      Map((Team.TEAM_1, TeamPoints(members._1.map(_.username), 0)), (Team.TEAM_2, TeamPoints(members._2.map(_.username), 0))),
      lobbyToLobbyId(lobby),
      lobby.members.map(elem => (elem.username, elem)).toMap)
  }
}


private[this] object SessionManagerMessage {

  sealed trait SessionManagerMessage

  case class StartMatch(firstPlayer: Option[UserInfo]) extends SessionManagerMessage
  case class EndMatchMessage(lastHandWinner: UserInfo) extends SessionManagerMessage
  case object Timeout extends SessionManagerMessage
  case object TimeoutId extends SessionManagerMessage

}