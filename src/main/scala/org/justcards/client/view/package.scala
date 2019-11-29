package org.justcards.client

import org.justcards.commons.{Card, LobbyId, UserId}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

package object view {

  object MenuChoice extends Enumeration {
    val CREATE_LOBBY: Value = Value(1, "Create a new lobby")
    val JOIN_LOBBY: Value = Value(2, "Join to existing lobby")
    val CREATE_GAME: Value = Value(3, "Create a new game")
  }

  object OptionConnectionFailed extends Enumeration {
    val TRY_TO_RECONNECT: Value = Value(1, "Try to reconnect")
    val QUIT: Value = Value(2, "Close App")
  }

  implicit def fromUserToString(user: UserId): String = user.name

  implicit def fromLobbyToString(lobby: LobbyId): String =
    "Lobby <" + lobby.id + "> created by " + lobby.owner + " | Game: " + lobby.game.name

  implicit def fromCardToString(card: Card): String = card.number + " " + card.seed

  implicit def fromIntToMenuChoice(choice: Int): MenuChoice.Value = MenuChoice(choice)

  implicit def fromIntToOptionConnectionFailed(choice: Int): OptionConnectionFailed.Value = OptionConnectionFailed(choice)

  implicit class RichFuture[A](future: Future[A]) {
    def onSuccessfulComplete(nextOperation: => A => Unit)(implicit executor: ExecutionContext): Unit =
      future onComplete {
        case Success(value) => nextOperation(value)
        case _ =>
      }
  }

  implicit class RichCards(cards: Set[Card]){
    def ordered: List[Card] = cards.toList sortBy(_.number) sortBy(_.seed)
  }

}
