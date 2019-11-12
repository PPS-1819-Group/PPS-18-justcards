package org.justcards.commons

object AppError extends Enumeration {
  val USER_ALREADY_PRESENT: Value  = Value("#1")
  val CONNECTION_LOST: Value = Value("#2")
  val SELECTION_NOT_AVAILABLE: Value = Value("#3")
  val CANNOT_CONNECT: Value = Value("#4")
  val MESSAGE_SENDING_FAILED: Value = Value("#5")
}