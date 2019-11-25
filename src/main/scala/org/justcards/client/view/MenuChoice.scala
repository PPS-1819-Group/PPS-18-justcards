package org.justcards.client.view

object MenuChoice extends Enumeration {
  val CREATE_LOBBY: Value = Value(1, "Create a new lobby")
  val JOIN_LOBBY: Value = Value(2, "Join to existing lobby")
  val CREATE_GAME: Value = Value(3, "Create a new game")
}
