package org.justcards.commons

case class GameId (name: String)
case class UserId (id: Long, name: String)
case class LobbyId (id: Long, owner: String, game: GameId)
case class Card(number: Int, seed: String)
case class TeamId(name: String)