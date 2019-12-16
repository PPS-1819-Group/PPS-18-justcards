package org.justcards.commons

package object actor_connection {

  /**
   * Message to wrap an external message.
   * @param message the external message
   */
  case class Outer(message: AppMessage)
}
