package org.justcards.commons

/**
 * Trait for messages that will be exchanged between application client and server.
 */
sealed trait AppMessage

/**
 * Message to indicate that an error occurred
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
case class AvailableGames(games: Set[GameId]) extends AppMessage

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
 * Message that contains all the available lobbies
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
 * Message to communicate information of current hand and field
 * @param handCards cards in your hand
 * @param fieldCards cards on the field
 */
case class Information(handCards: Set[Card], fieldCards: List[Card]) extends AppMessage

/**
 * Message to indicate to choose the Briscola
 * @param seeds the available seeds
 * @param timeout the timeout given to choose the Briscola
 */
case class ChooseBriscola(seeds: Set[String], timeout: Int) extends AppMessage

/**
 * Message to indicate the chosen Briscola
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
 * Message to indicate that is your turn
 *
 * @param handCards cards in your hand
 * @param fieldCards cards on the field
 * @param timeout time limit to play your card
 */
case class Turn(handCards: Set[Card], fieldCards: List[Card], timeout: Int) extends AppMessage

/**
 * Message to indicate the card you want to play
 * @param card the card you want to play
 */
case class Play(card: Card) extends AppMessage

/**
 * Message to indicate that your turn is over
 * @param card the card you played
 */
case class Played(card: Card) extends AppMessage

/**
 * Message to indicate that the timeout has exceeded
 */
case class TimeoutExceeded(option: String = "") extends AppMessage

/**
 * Message to indicate who won the hand
 * @param player the winner
 */
case class HandWinner(player: UserId) extends AppMessage

/**
 * Message to indicate the team who won the match
 * @param team the winner
 * @param matchPoints the points of the current match
 * @param totalPoints the total points of the current session
 */
case class MatchWinner(team: TeamId, matchPoints: (Int, Int), totalPoints: (Int, Int)) extends AppMessage

/**
 * Message to indicate the team who won the game
 * @param team the winner
 */
case class GameWinner(team: TeamId) extends AppMessage

/**
 * Message to get out of a lobby
 * @param lobby the lobby to leave
 */
case class OutOfLobby(lobby: LobbyId) extends AppMessage