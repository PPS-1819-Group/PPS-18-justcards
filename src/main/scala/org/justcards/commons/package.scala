package org.justcards

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
  }
}