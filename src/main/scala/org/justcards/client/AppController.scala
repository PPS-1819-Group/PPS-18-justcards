package org.justcards.client

trait AppController {
  def login(nickname: String): Unit
}

case class AppControllerImpl() extends AppController {
  override def login(nickname: String): Unit = println("Controller " + nickname)
}