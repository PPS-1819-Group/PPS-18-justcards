package org.justcards.commons

import akka.actor.ActorRef
import akka.io.Tcp.{Received, Write}
import akka.util.ByteString
import play.api.libs.json.{Json, OFormat}

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

object AppMessage {

  /*
   * For each message is mandatory to create a Writes[...] and Reads[...]
   * implicits that will help Json class to do the automatic conversion
   * from class to JSValue.
   * We need it to be private to the companion object because otherwise
   * it will raise an exception due to the confusion between the class specific
   * element and the trait one.
   */
  private[this] implicit val gameIdFormat: OFormat[GameId] = Json.format[GameId]
  private[this] implicit val userIdFormat: OFormat[UserId] = Json.format[UserId]
  private[this] implicit val lobbyIdFormat: OFormat[LobbyId] = Json.format[LobbyId]

  private[this] implicit val loginFormat: OFormat[LogIn] = Json.format[LogIn]
  private[this] implicit val logoutFormat: OFormat[LogOut] = Json.format[LogOut]
  private[this] implicit val loggedFormat: OFormat[Logged] = Json.format[Logged]
  private[this] implicit val retrAvailGamesFormat: OFormat[RetrieveAvailableGames] = Json.format[RetrieveAvailableGames]
  private[this] implicit val availGamesFormat: OFormat[AvailableGames] = Json.format[AvailableGames]
  private[this] implicit val createLobbyFormat: OFormat[CreateLobby] = Json.format[CreateLobby]
  private[this] implicit val lobbyCreatedFormat: OFormat[LobbyCreated] = Json.format[LobbyCreated]
  private[this] implicit val retrAvailLobbiesFormat: OFormat[RetrieveAvailableLobbies] = Json.format[RetrieveAvailableLobbies]
  private[this] implicit val availLobbiesFormat: OFormat[AvailableLobbies] = Json.format[AvailableLobbies]
  private[this] implicit val joinLobbyFormat: OFormat[JoinLobby] = Json.format[JoinLobby]
  private[this] implicit val lobbyJoinedFormat: OFormat[LobbyJoined] = Json.format[LobbyJoined]
  private[this] implicit val lobbyUpdateFormat: OFormat[LobbyUpdate] = Json.format[LobbyUpdate]
  private[this] implicit val gameStartedFormat: OFormat[GameStarted] = Json.format[GameStarted]
  private[this] implicit val errorOccurredFormat: OFormat[ErrorOccurred] = Json.format[ErrorOccurred]

  /*
   * Implicit for the conversion of the trait
   */
  implicit val messagesFormat: OFormat[AppMessage] = Json.format[AppMessage]

  /*
   * Utilities for conversions of
   * String -> ByteString
   * ByteString -> String
   * AppMessage -> String
   */
  implicit def fromStringToByteString(msg: String): ByteString = ByteString(msg)
  implicit def fromByteStringToString(msg: ByteString): String = msg.utf8String
  implicit def fromAppMessageToString(msg: AppMessage): ByteString = fromStringToByteString(Json.toJson(msg).toString())


  implicit def fromReceivedDataToMessage(msg: Received): AppMessage = extractMessage(msg.data)


  /*
   * Extracting the message from a string that HAS TO BE a json object
   * or it will raise an exception because it can't do the conversion.
   */
  def extractMessage(originalMessage: String): AppMessage = {
    val elemParsed = Json.parse(originalMessage)
    val elem = Json.fromJson[AppMessage](elemParsed)
    if(elem.isSuccess) elem.get else null
  }

  implicit class MyPersonalSender(actor: ActorRef){
    def ==>(message: AppMessage): Unit = actor ! Write(message)
  }
}