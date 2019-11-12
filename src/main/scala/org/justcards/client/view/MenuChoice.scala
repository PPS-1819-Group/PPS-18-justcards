package org.justcards.client.view

object MenuChoice extends Enumeration {
  val createLobby: Value = Value(1, "Create a new lobby")
  val joinLobby: Value = Value(2, "Join to existing lobby")
  val createGame: Value = Value(3, "Create a new game")
}
