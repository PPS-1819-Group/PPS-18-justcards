package org.justcards.client.view

import akka.actor.{ActorRef, Props}
import org.justcards.commons.AppError._
import org.justcards.commons.games_rules.Rule
import org.justcards.commons.{Card, GameId, LobbyId, TeamId, UserId}

trait View extends (ActorRef => Props)

object View {

  def apply(): View = ConsoleView()

  sealed trait ViewMessage
  case class ShowError(error: AppError) extends ViewMessage
  case object ShowUsernameChoice extends ViewMessage
  case object ShowMenu extends ViewMessage
  case object MoveWasCorrect extends ViewMessage
  case object ShowTimeForMoveExceeded extends ViewMessage
  case object ShowGameCreated extends ViewMessage
  case class ShowLobbyCreation(games: Set[GameId]) extends ViewMessage
  case class ShowLobbies(lobbies: Set[(LobbyId, Set[UserId])]) extends ViewMessage
  case class ShowCreatedLobby(lobby: LobbyId) extends ViewMessage
  case class ShowJoinedLobby(lobby: LobbyId, members: Set[UserId]) extends ViewMessage
  case class ShowLobbyUpdate(lobby: LobbyId, members: Set[UserId]) extends ViewMessage
  case class ShowGameStarted(players: List[(UserId, TeamId)]) extends ViewMessage
  case class ShowGameInformation(handCards: Set[Card], fieldCards: List[Card]) extends ViewMessage
  case class ViewChooseBriscola(availableBriscola: Set[String], timeout: Int) extends ViewMessage
  case class ShowTurn(handCards: Set[Card], fieldCards: List[Card], timeout: Int) extends ViewMessage
  case class ShowHandWinner(player: UserId) extends ViewMessage
  case class ShowMatchWinner(winnerTeam: TeamId, matchPoints: (Int, Int), totalPoints: (Int, Int)) extends ViewMessage
  case class ShowGameWinner(team: TeamId) extends ViewMessage
  case class ShowChosenBriscola(seed: String, number: Option[Int] = None) extends ViewMessage
  case class ShowGameCreation(rules: Set[Rule.Value], cards: Set[Card], seeds: Set[String]) extends ViewMessage
  case class ShowNotValidRules(rules: Set[Rule.Value]) extends ViewMessage
  case class ShowAvailableGames(games: Set[(GameId, Long)]) extends ViewMessage

  private[this] val HAND_WON_FINAL = "taken all the cards in the field!"

  val INPUT_SYMBOL: String = "> "
  val MENU_TITLE: String = "MENU"
  val ERROR_TITLE: String = "ERROR"
  val LOBBY_BY_ID_TITLE: String = "LOBBY BY ID - Insert the lobby ID you want to join"
  val LOBBY_BY_FILTER_TITLE: String = "LOBBY LIST - Choose the filter you want to filter lobbies with"
  val LOBBY_CREATION_TITLE: String = "LOBBY CREATION - Choose the game you want to play"
  val LOBBY_LIST_TITLE: String = "LOBBY LIST - Choose the lobby you want to join"
  val MATCH_RESULTS_TITLE: String = "MATCH RESULTS"
  val GAMES_TITLE: String = "AVAILABLE GAMES - These are all the available games on the platform"
  val EXIT: String = "exit"
  val DEFAULT_LOBBIES_MESSAGE = "<< Currently there are no available lobbies >>"
  val LOBBY_MESSAGE: String = "If you want to exit from the lobby, write " concat EXIT
  val HAND_WON: String = "You've " concat HAND_WON_FINAL
  val GAME_WON: String = "You've won the game!!!"
  val TIME_IS_UP: String = "Time'up! The game will decide for you"

  val LOBBY_CREATED_MESSAGE: LobbyId => String = "Your lobby has been created! ID = " + _.id
  val BRISCOLA_NO_CARD: String => String = "<< Current Briscola is " concat _ concat " >>"
  val BRISCOLA_WITH_CARD: Card => String = card =>
    BRISCOLA_NO_CARD(card) concat " | Briscola card is " concat fromCardToString(card)
  val LOBBY_JOINED_MESSAGE: LobbyId => String = "You joined lobby " + _.id
  val GAME_STARTED: (TeamId, Option[UserId]) => String = (team, mate) => {
   val msg = "The game has started! You're on team " + team.name
   if(mate isDefined) msg concat " and your teammate is " + mate.get.name
   else msg
  }
  val HAND_LOST: UserId => String = _.name concat " has " concat HAND_WON_FINAL
  val GAME_LOST: TeamId => String = "You've lost! " concat _.name concat " wins the game"
  val MATCH_RESULTS: ((Int,Int)) => String = results =>
    "Match ends! Results: " + results._1 + " - " + results._2
  val MATCH_ENDS: (TeamId, (Int,Int)) => String = (winnerTeam, teamPoints) =>
    winnerTeam.name + " wins! " + teamPoints._1 + " - " + teamPoints._2
  val UNKNOWN_ERROR: AppError => String = "Unknown error: " + _

  private val START_ERROR: String = "Error: "

  val ERROR_CONNECTION_LOST: String = START_ERROR concat "Connection Lost"
  val ERROR_CANNOT_CONNECT: String = START_ERROR concat "I can't connect"
  val ERROR_LAST_MESSAGE_LOST: String = START_ERROR concat "Last message didn't arrive"

  val ERROR_USERNAME_ALREADY_USED: String = START_ERROR concat "Username already used"
  val ERROR_USER_NOT_LOGGED: String = START_ERROR concat "You're not logged"
  val ERROR_USER_ALREADY_LOGGED: String = START_ERROR concat "You're already logged"
  val ERROR_USER_ALREADY_IN_LOBBY: String = START_ERROR concat "You're already in a lobby"
  val ERROR_USER_WRONG_USERNAME: String = START_ERROR concat "Action not allowed with this username"

  val ERROR_GAME_NOT_EXIST: String = START_ERROR concat "Selected game doesn't exist"
  val ERROR_LOBBY_NOT_EXIST: String = START_ERROR concat "Selected lobby doesn't exist"
  val ERROR_LOBBY_FULL: String = START_ERROR concat "Selected lobby is already full"

  val ERROR_GAME_EMPTY_NAME: String = START_ERROR concat "The name of the game can't be empty!"
  val ERROR_GAME_MISSING_RULES: String = START_ERROR concat "Some rules are missing! The game can't be created"
  val ERROR_GAME_WRONG_RULES: String = START_ERROR concat "Some rules are wrong! Change them"
  val ERROR_GAME_ALREADY_EXISTS: String = START_ERROR concat "The game already exists! Choose another name"

  val ERROR_WRONG_CARD: String = "You can't play this card"
  val ERROR_WRONG_BRISCOLA: String = "This Briscola doesn't exist"

  val ERROR_SERVER: String = START_ERROR concat "An error has occured server side, try again."
  val ERROR_WRONG_CHOICE: String = START_ERROR concat "The selected choice is not available"

  def errorMessage(error: AppError): String = error match {
    case CONNECTION_LOST => ERROR_CONNECTION_LOST
    case CANNOT_CONNECT => ERROR_CANNOT_CONNECT
    case MESSAGE_SENDING_FAILED => ERROR_LAST_MESSAGE_LOST
    case USER_ALREADY_PRESENT => ERROR_USERNAME_ALREADY_USED
    case USER_NOT_LOGGED => ERROR_USER_NOT_LOGGED
    case USER_ALREADY_LOGGED => ERROR_USER_ALREADY_LOGGED
    case USER_ALREADY_IN_A_LOBBY => ERROR_USER_ALREADY_IN_LOBBY
    case USER_WRONG_USERNAME => ERROR_USER_WRONG_USERNAME
    case GAME_NOT_EXISTING => ERROR_GAME_NOT_EXIST
    case LOBBY_NOT_EXISTING => ERROR_GAME_NOT_EXIST
    case LOBBY_FULL => ERROR_LOBBY_FULL
    case SELECTION_NOT_AVAILABLE => ERROR_WRONG_CHOICE
    case BRISCOLA_NOT_VALID => ERROR_WRONG_BRISCOLA
    case CARD_NOT_VALID => ERROR_WRONG_CARD
    case SERVER_ERROR => ERROR_SERVER
    case GAME_EMPTY_NAME => ERROR_GAME_EMPTY_NAME
    case GAME_MISSING_RULES => ERROR_GAME_MISSING_RULES
    case GAME_RULES_NOT_VALID => ERROR_GAME_WRONG_RULES
    case GAME_ALREADY_EXISTS => ERROR_GAME_ALREADY_EXISTS
    case _ => UNKNOWN_ERROR(error)
  }
}
