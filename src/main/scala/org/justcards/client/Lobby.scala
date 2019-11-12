package org.justcards.client

import org.justcards.commons.LobbyId

case class Lobby(id: LobbyId, creator: String, participants: Int) {
  override def toString: String = id + " created by " + creator + "(" + participants + "/4)"
}
