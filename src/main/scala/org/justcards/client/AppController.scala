package org.justcards.client

trait AppController {
  def login(nickname: String): Unit
  def selectionMenu(choice: MenuChoice.Value): Unit
}

case class AppControllerImpl() extends AppController {
  override def login(nickname: String): Unit = println("Controller " + nickname)

  override def selectionMenu(choice: MenuChoice.Value): Unit = println("Controller " + choice)
}