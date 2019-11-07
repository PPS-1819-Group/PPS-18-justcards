package org.justcards.commons

sealed trait Id {
  def id: Long
}

case class GameId(id: Long, name: String) extends Id
case class UserId(id: Long, name: String) extends Id
case class LobbyId(id: Long) extends Id
