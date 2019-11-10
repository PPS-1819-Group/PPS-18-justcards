package org.justcards.client.controller

import akka.actor.{Actor, Props, Stash}
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.view.{View, ViewFactory}
import org.justcards.commons._

trait AppController {
  def login(username: String): Unit
}

object AppController {
  def apply(connectionManager: ConnectionManager, viewFactory: ViewFactory) =
    Props(classOf[AppControllerActor], connectionManager, viewFactory)

  private[this] class AppControllerActor(connectionManager: ConnectionManager, viewFactory: ViewFactory) extends Actor with Stash with AppController {

    private val connectionManagerActor = context.actorOf(connectionManager(self))
    private val view: View = viewFactory(this)

    override def receive: Receive = init

    override def login(username: String): Unit = connectionManagerActor ! LogIn(username)

    private def init: Receive = {
      case ErrorOccurred(message) => view error message
      case Logged(_) =>
        unstashAll()
        context become logged
        view logged()
      case _ => stash()
    }

    private def logged: Receive = {
      case _ => stash()
    }

  }
}