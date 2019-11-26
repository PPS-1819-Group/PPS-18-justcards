package org.justcards.commons.actor_connection

import akka.actor.ActorRef
import akka.io.Tcp.{Received, Write}
import akka.util.ByteString
import org.justcards.commons._
import play.api.libs.json.{Json, OFormat}

trait ActorWithTcp extends ActorWithConnection {

  import org.justcards.commons.actor_connection.ActorWithTcp._

  override def parse: Receive = {
    case Received(data) => self ! Outer(extractMessage(data))
  }

  abstract override private[actor_connection] def tellWithConnection(actor: ActorRef, message: Any): Unit = message match {
    case msg: AppMessage => super.tellWithConnection(actor,Write(msg))
    case msg => super.tellWithConnection(actor,msg)
  }
}

object ActorWithTcp {

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
  private[this] implicit val cardFormat: OFormat[Card] = Json.format[Card]
  private[this] implicit val teamIdFormat: OFormat[TeamId] = Json.format[TeamId]

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
  private[this] implicit val informationFormat: OFormat[Information] = Json.format[Information]
  private[this] implicit val chooseBriscolaFormat: OFormat[ChooseBriscola] = Json.format[ChooseBriscola]
  private[this] implicit val correctBriscolaFormat: OFormat[CorrectBriscola] = Json.format[CorrectBriscola]
  private[this] implicit val briscolaFormat: OFormat[Briscola] = Json.format[Briscola]
  private[this] implicit val turnFormat: OFormat[Turn] = Json.format[Turn]
  private[this] implicit val playFormat: OFormat[Play] = Json.format[Play]
  private[this] implicit val playedFormat: OFormat[Played] = Json.format[Played]
  private[this] implicit val timeoutExceededFormat: OFormat[TimeoutExceeded] = Json.format[TimeoutExceeded]
  private[this] implicit val handWinnerFormat: OFormat[HandWinner] = Json.format[HandWinner]
  private[this] implicit val matchWinnerFormat: OFormat[MatchWinner] = Json.format[MatchWinner]
  private[this] implicit val gameWinnerFormat: OFormat[GameWinner] = Json.format[GameWinner]
  private[this] implicit val outOfLobbyFormat: OFormat[OutOfLobby] = Json.format[OutOfLobby]
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

}
