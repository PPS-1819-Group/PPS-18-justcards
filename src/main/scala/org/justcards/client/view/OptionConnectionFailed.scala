package org.justcards.client.view

object OptionConnectionFailed extends Enumeration {
  val tryReconnect: Value = Value(1, "Try to reconnect")
  val quit: Value = Value(2, "Close Game")
}
