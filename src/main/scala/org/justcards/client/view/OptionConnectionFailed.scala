package org.justcards.client.view

object OptionConnectionFailed extends Enumeration {
  val TRY_TO_RECONNECT: Value = Value(1, "Try to reconnect")
  val QUIT: Value = Value(2, "Close App")
}
