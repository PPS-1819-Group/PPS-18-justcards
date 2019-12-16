package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import org.justcards.commons.{ErrorOccurred, Logged}
import org.justcards.server.Commons.UserInfo
import org.justcards.server.user_manager.UserManagerMessage._

/**
 * Actor that deals with players.
 * @param playerDatabase the PlayerDatabase
 */
private[user_manager] class PlayerManager(playerDatabase: PlayerDatabase) extends Actor {

  import org.justcards.commons.AppError._

  override def receive: Receive = defaultBehaviour(playerDatabase)

  private def defaultBehaviour(users: PlayerDatabase): Receive = {
    case UserLogIn(message, user) =>
      val loggedUser = users find(userData =>
        userData._2 == user || userData._1 == message.username
      ) map(_._1)
      if (loggedUser.isDefined && loggedUser.get == message.username)
        user ! ErrorOccurred(USER_ALREADY_PRESENT)
      else if (loggedUser.isDefined)
        user ! ErrorOccurred(USER_ALREADY_LOGGED)
      else {
        user ! Logged(message.username)
        val updatedUsers = users + (message.username -> user)
        context become defaultBehaviour(updatedUsers)
      }
    case UserLogout(username, user) =>
      if (users contains (username -> user))
        context become defaultBehaviour(users - username)
    case RetrieveAllPlayers(msgSender) =>
      val usersSet: Set[UserInfo] = users map(data => UserInfo(data._1, data._2))
      msgSender ! Players(usersSet)
    case PlayerLogged(user) =>
      val userRes = users.find(_._2 == user).map(_._1)
      if(userRes.isDefined) sender() ! PlayerLoggedResult(found = true, userRes.get)
      else sender() ! PlayerLoggedResult(found = false, "")
  }
}

private[user_manager] object PlayerManager {
  def apply(): Props = Props(classOf[PlayerManager], PlayerDatabase.createSetPlayerDatabase())
  def apply(playerDatabase: PlayerDatabase): Props = Props(classOf[PlayerManager], playerDatabase)
}

/**
  * Trait for classes that should contain informations about the players.
  */
trait PlayerDatabase extends Set[(String, ActorRef)] {
  /**
   * Add a new player.
   * @param value the new player information
   * @return the new PlayerDatabase created
   */
  def +(value: (String, ActorRef)): PlayerDatabase

  /**
   * Remove a player
   * @param value the player information
   * @return the new PlayerDatabase without the given player
   */
  def -(value: (String, ActorRef)): PlayerDatabase

  /**
   * Remove a player knowing only its username
   * @param value the player username
   * @return the new PlayerDatabase without the given player
   */
  def -(value: String): PlayerDatabase
}

object PlayerDatabase {

  def createSetPlayerDatabase(): PlayerDatabase = PlayerDbSet()

  private[this] case class PlayerDbSet(set: Set[(String, ActorRef)] = Set()) extends PlayerDatabase {

    override def +(value: (String, ActorRef)): PlayerDatabase = PlayerDbSet(set + value)

    override def -(username: String): PlayerDatabase = PlayerDbSet(set filterNot(_._1 == username))

    override def contains(elem: (String, ActorRef)): Boolean = set contains elem

    override def -(elem: (String, ActorRef)): PlayerDatabase = PlayerDbSet(set - elem)

    override def iterator: Iterator[(String, ActorRef)] = set.iterator
  }

}

