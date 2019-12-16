package org.justcards.commons

import org.justcards.commons.games_rules.GameRulesSettings
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

/**
 * Trait for messages that will be exchanged between application client and server.
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value = classOf[LogIn], name = "LogIn"),
    new JsonSubTypes.Type(value = classOf[LogOut], name = "LogOut"),
    new JsonSubTypes.Type(value = classOf[Logged], name = "Logged"),
    new JsonSubTypes.Type(value = classOf[RetrieveAvailableGames], name = "RetrieveAvailableGames"),
    new JsonSubTypes.Type(value = classOf[AvailableGames], name = "AvailableGames"),
    new JsonSubTypes.Type(value = classOf[CreateLobby], name = "CreateLobby"),
    new JsonSubTypes.Type(value = classOf[LobbyCreated], name = "LobbyCreated"),
    new JsonSubTypes.Type(value = classOf[RetrieveAvailableLobbies], name = "RetrieveAvailableLobbies"),
    new JsonSubTypes.Type(value = classOf[AvailableLobbies], name = "AvailableLobbies"),
    new JsonSubTypes.Type(value = classOf[JoinLobby], name = "JoinLobby"),
    new JsonSubTypes.Type(value = classOf[LobbyJoined], name = "LobbyJoined"),
    new JsonSubTypes.Type(value = classOf[LobbyUpdate], name = "LobbyUpdate"),
    new JsonSubTypes.Type(value = classOf[GameStarted], name = "GameStarted"),
    new JsonSubTypes.Type(value = classOf[Information], name = "Information"),
    new JsonSubTypes.Type(value = classOf[ChooseBriscola], name = "ChooseBriscola"),
    new JsonSubTypes.Type(value = classOf[CorrectBriscola], name = "CorrectBriscola"),
    new JsonSubTypes.Type(value = classOf[Briscola], name = "Briscola"),
    new JsonSubTypes.Type(value = classOf[Turn], name = "Turn"),
    new JsonSubTypes.Type(value = classOf[Play], name = "Play"),
    new JsonSubTypes.Type(value = classOf[Played], name = "Played"),
    new JsonSubTypes.Type(value = classOf[TimeoutExceeded], name = "TimeoutExceeded"),
    new JsonSubTypes.Type(value = classOf[HandWinner], name = "HandWinner"),
    new JsonSubTypes.Type(value = classOf[MatchWinner], name = "MatchWinner"),
    new JsonSubTypes.Type(value = classOf[GameWinner], name = "GameWinner"),
    new JsonSubTypes.Type(value = classOf[OutOfLobby], name = "OutOfLobby"),
    new JsonSubTypes.Type(value = classOf[ErrorOccurred], name = "ErrorOccurred"),
    new JsonSubTypes.Type(value = classOf[CreateGame], name = "CreateGame"),
    new JsonSubTypes.Type(value = classOf[GameCreated], name = "GameCreated")
  ))
sealed trait AppMessage extends JsonSerializable

/**
 * Message to indicate that an error occurred.
 * @param error the error
 */
case class ErrorOccurred(error: String) extends AppMessage

/**
 * Message to log in to the system.
 * @param username user's username
 */
case class LogIn(username: String) extends AppMessage

/**
 * Message to log out of the system.
 * @param username user's username
 */
case class LogOut(username: String) extends AppMessage

/**
 * Message to indicate that the login is successful.
 */
case class Logged(username: String = "") extends AppMessage

/**
 * Message to request to the server all the available games.
 */
case class RetrieveAvailableGames(options: String = "") extends AppMessage

/**
 * Message that contains all the available games.
 * @param games all the available games
 */
case class AvailableGames(games: Set[(GameId, Long)]) extends AppMessage

/**
 * Message to create a lobby.
 * @param game the chosen game
 */
case class CreateLobby(game: GameId) extends AppMessage

/**
 * Message to indicate that a lobby has been created.
 * @param lobby the current lobby information
 */
case class LobbyCreated(lobby: LobbyId) extends AppMessage

/**
 * Message to ask which are the available lobbies, with possible filters.
 * @param gameName the game you want to play
 * @param ownerName the name of the owner of the lobby
 */
case class RetrieveAvailableLobbies(gameName: String = "", ownerName: String = "") extends AppMessage

/**
 * Message that contains all the available lobbies.
 * @param lobbies all the available lobbies
 */
case class AvailableLobbies(lobbies: Set[(LobbyId, Set[UserId])]) extends AppMessage

/**
 * Message to use to join a lobby.
 * @param lobby the lobby that you want to join in
 */
case class JoinLobby(lobby: LobbyId) extends AppMessage

/**
 * Message to indicate that you have joined the lobby.
 * @param lobby the lobby information
 * @param members current lobby members
 */
case class LobbyJoined(lobby: LobbyId, members: Set[UserId]) extends AppMessage

/**
 * Message to indicate that the status of the lobby is changed.
 * @param lobby the lobby information
 * @param members current lobby members
 */
case class LobbyUpdate(lobby: LobbyId, members: Set[UserId]) extends AppMessage

/**
 * Message to indicate that a game is started.
 * @param players the list of players and their respective teams
 */
case class GameStarted(players: List[(UserId, TeamId)]) extends AppMessage

/**
 * Message to communicate information of current hand and field.
 * @param handCards cards in your hand
 * @param fieldCards cards on the field
 */
case class Information(handCards: Set[Card], fieldCards: List[Card]) extends AppMessage

/**
 * Message to indicate to choose the Briscola.
 * @param seeds the available seeds
 * @param timeout the timeout given to choose the Briscola
 */
case class ChooseBriscola(seeds: Set[String], timeout: Int) extends AppMessage

/**
 * Message to indicate the chosen Briscola.
 * @param seed the chosen Briscola
 */
case class Briscola(seed: String) extends AppMessage

/**
 * Message to indicate that the chosen Briscola was correct.
 * @param seed the Briscola seed
 * @param number the Briscola number if present, None otherwise
 */
case class CorrectBriscola(seed: String, number: Option[Int] = None) extends AppMessage

/**
 * Message to indicate that is your turn.
 * @param handCards cards in your hand
 * @param fieldCards cards on the field
 * @param timeout time limit to play your card
 */
case class Turn(handCards: Set[Card], fieldCards: List[Card], timeout: Int) extends AppMessage

/**
 * Message to indicate the card you want to play.
 * @param card the card you want to play
 */
case class Play(card: Card) extends AppMessage

/**
 * Message to indicate that your turn is over.
 * @param card the card you played
 */
case class Played(card: Card) extends AppMessage

/**
 * Message to indicate that the timeout has exceeded.
 */
case class TimeoutExceeded(option: String = "") extends AppMessage

/**
 * Message to indicate who won the hand.
 * @param player the winner
 */
case class HandWinner(player: UserId) extends AppMessage

/**
 * Message to indicate the team who won the match.
 * @param team the winner
 * @param matchPoints the points of the current match
 * @param totalPoints the total points of the current session
 */
case class MatchWinner(team: TeamId, matchPoints: (Int, Int), totalPoints: (Int, Int)) extends AppMessage

/**
 * Message to indicate the team who won the game.
 * @param team the winner
 */
case class GameWinner(team: TeamId) extends AppMessage

/**
 * Message to get out of a lobby.
 * @param lobby the lobby to leave
 */
case class OutOfLobby(lobby: LobbyId) extends AppMessage

/**
 * Message to create a game.
 * @param name the name of the new game
 * @param rules the rules of the new game
 */
case class CreateGame(name: String, rules: GameRulesSettings) extends AppMessage

/**
 * Message to indicate that the game has been created.
 * @param game the new game
 */
case class GameCreated(game: GameId) extends AppMessage