package org.justcards

import org.justcards.commons.AppError._

package object commons {
  val SERVER_SYSTEM_NAME: String = "justCardsServer"
  val SERVER_NAME: String = SERVER_SYSTEM_NAME

  implicit class RichString(value: String) {
    def toOptionInt: Option[Int] =
      try {
        Some(value.toInt)
      } catch {
        case _: Exception => None
      }

    def isGameError: Boolean =
      value == CANNOT_CREATE_GAME.toString ||
      value == GAME_EMPTY_NAME.toString ||
      value == GAME_RULES_NOT_VALID.toString ||
      value == GAME_MISSING_RULES.toString ||
      value == GAME_ALREADY_EXISTS.toString

  }
}