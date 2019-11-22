package org.justcards.commons

case class GameId (name: String)
case class UserId (id: Long, name: String)
case class LobbyId (id: Long, owner: String, game: GameId) {
  override def toString: String = "Lobby " + id + " of " + game.name + " created by " + owner
}
