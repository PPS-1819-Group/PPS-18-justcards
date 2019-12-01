package org.justcards.client

import org.justcards.commons.{Card, LobbyId, UserId}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

package object view {

  object MenuChoice extends Enumeration {
    type MenuChoice = MenuChoice.Value
    val CREATE_LOBBY: Value = Value(1, "Create a new lobby")
    val LIST_LOBBY: Value = Value(2, "List existing lobbies")
    val LIST_LOBBY_WITH_FILTERS: Value = Value(3, "Search lobbies by filter")
    val JOIN_LOBBY_BY_ID: Value = Value(4, "Join lobby")
    val CREATE_GAME: Value = Value(5, "Create a new game")
  }

  object OptionConnectionFailed extends Enumeration {
    type OptionConnectionFailed = OptionConnectionFailed.Value
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
