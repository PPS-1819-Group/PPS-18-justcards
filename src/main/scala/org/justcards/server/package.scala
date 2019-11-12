package org.justcards

package object server {

  def log(message: String): Unit = println("[" + java.time.LocalDateTime.now + "]" + message)

}
