package org.justcards.commons

object AppError extends Enumeration {

  val CONNECTION_LOST: Value = Value("#1")
  val CANNOT_CONNECT: Value = Value("#2")
  val MESSAGE_SENDING_FAILED: Value = Value("#3")

  val USER_ALREADY_PRESENT: Value  = Value("#4")
  val USER_NOT_LOGGED: Value = Value("#5")
  val USER_ALREADY_LOGGED: Value = Value("#6")
  val USER_ALREADY_IN_A_LOBBY: Value = Value("#7")
  val USER_WRONG_USERNAME: Value = Value("#8") //logout with a given wrong username

  val GAME_NOT_EXISTING: Value = Value("#9")
  val LOBBY_NOT_EXISTING: Value = Value("#10")
  val LOBBY_FULL: Value = Value("#11")

  val BRISCOLA_NOT_VALID: Value = Value("#12")
  val CARD_NOT_VALID: Value = Value("#13")

  val SELECTION_NOT_AVAILABLE: Value = Value("#99")

  implicit def fromAppErrorToString(error: AppError.Value):String = error.toString

}