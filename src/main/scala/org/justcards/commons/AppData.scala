package org.justcards.commons

trait JsonSerializable

case class GameId (name: String) extends JsonSerializable
case class UserId (id: Long, name: String) extends JsonSerializable
case class LobbyId (id: Long, owner: String = "", game: GameId = GameId("")) extends JsonSerializable
case class Card(number: Int, seed: String) extends JsonSerializable
case class TeamId(name: String) extends JsonSerializable
