package org.justcards.commons

/**
  * Trait for messages that will be exchanged between application client and server.
  */
sealed trait AppMessage

/**
  * Message to indicate that an error occurred
  * @param message the error
  */
case class ErrorOccurred(message: String) extends AppMessage

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
  * Message to ask which are the available lobbies.
  */
case class RetrieveAvailableLobbies(options: String = "") extends AppMessage

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
  */
case class GameStarted(options: String = "") extends AppMessage