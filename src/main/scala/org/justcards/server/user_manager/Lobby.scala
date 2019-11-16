package org.justcards.server.user_manager

import org.justcards.commons.{GameId, LobbyId}
import org.justcards.server.user_manager.UserManagerMessage.UserInfo

/**
  * Class that represents all the information of a lobby
  */
sealed trait Lobby {
  /**
    * Getter
    * @return the lobby is
    */
  def id: Long

  /**
    * Getter
    * @return the lobby owner
    */
  def owner: UserInfo

  /**
   * Getter
   * @return the lobby game
   */
  def game: GameId

  /**
    * Add a new owner to the lobby
    * @param newOwner the new owner of the lobby
    * @return a new lobby with the new lobby
    */
  def -->(newOwner: UserInfo): Lobby

  /**
    * Getter.
    * @return the lobby members
    */
  def members: Set[UserInfo]

  /**
    * Add a member to the lobby
    * @param member the new member
    * @return a new lobby that contains the new member
    */
  def +(member: UserInfo): Lobby

  /**
    * Remove a member from the lobby
    * @param member the member to remove
    * @return a new lobby where the member is not present if the lobby contains more than a user,
    *         nothing otherwise
    */
  def -(member: UserInfo): Option[Lobby]

  /**
    * Know if the lobby is full
    * @return if the lobby is full
    */
  def isFull: Boolean
}

object Lobby {

  /**
    * Creates a new lobby
    * @param id the lobby id
    * @param owner the owner of the lobby
    * @param game the game
    * @return a new lobby
    */
  def apply(id: Long, owner: UserInfo, game: GameId): Lobby = LobbyImpl(id, owner, game, Set(owner))

  implicit def lobbyToLobbyId(lobby: Lobby): LobbyId = LobbyId(lobby.id, lobby.owner.username, lobby.game)

  /**
    * Maximum capacity of a lobby
    */
  val MAX_LOBBY_MEMBERS: Int = 4

  private[this] case class LobbyImpl(id: Long, owner: UserInfo, game: GameId,
                                     members: Set[UserInfo]) extends Lobby {

    override def -->(newOwner: UserInfo): Lobby = LobbyImpl(id, newOwner, game, members)

    override def +(member: UserInfo): Lobby = LobbyImpl(id, owner, game, members + member)

    override def -(member: UserInfo): Option[Lobby] = member match {
      case `owner` if members.size == 1 => None
      case `owner` =>
        val members = this.members - member
        Some(LobbyImpl(id, members.head, game, members))
      case _ => Some(LobbyImpl(id, owner, game, members - member))
    }

    override def isFull: Boolean = members.size == MAX_LOBBY_MEMBERS

  }
}

/**
  * Trait for classes that should realise the database of the lobby
  */
trait LobbyDatabase extends Set[Lobby] {
  def +(value: Lobby): LobbyDatabase
  def -(value: Lobby): LobbyDatabase
  def contains(elem: Long): Boolean
  def apply(value: Long): Lobby
}

object LobbyDatabase {

  def createMapLobbyDatabase(): LobbyDatabase = LobbyDbMap()

  private[this] case class LobbyDbMap(map: Map[Long, Lobby] = Map()) extends LobbyDatabase {
    override def +(value: Lobby): LobbyDatabase = LobbyDbMap(map + (value.id -> value))
    override def -(value: Lobby): LobbyDatabase = LobbyDbMap(map - value.id)
    override def contains(elem: Lobby): Boolean = map contains elem.id
    override def contains(elem: Long): Boolean = map contains elem
    override def foreach[U](f: Lobby => U): Unit = map.values foreach f
    override def apply(value: Long): Lobby = map(value)
    override def iterator: Iterator[Lobby] = map.values.iterator
  }

}
