package org.justcards.commons

/**
 * Trait for classes that has to be serialized with the system Serializer.
 */
trait JsonSerializable

/**
 * Information about the game.
 * @param name the game name
 */
case class GameId (name: String) extends JsonSerializable

/**
 * Information about the user.
 * @param id the id
 * @param name the username
 */
case class UserId (id: Long, name: String) extends JsonSerializable

/**
 * Information about the lobby.
 * @param id the id
 * @param owner the owner
 * @param game the game chosen
 */
case class LobbyId (id: Long, owner: String = "", game: GameId = GameId("")) extends JsonSerializable

/**
 * Class that represent a card.
 * @param number the number of the card
 * @param seed the seed
 */
case class Card(number: Int, seed: String) extends JsonSerializable

/**
 * Information about a team.
 * @param name the team name
 */
case class TeamId(name: String) extends JsonSerializable
