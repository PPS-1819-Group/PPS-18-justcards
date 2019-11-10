package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import org.justcards.client.TestView.TestViewImpl
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.controller.AppController
import org.justcards.client.view.{View, ViewFactory}
import org.justcards.commons.actor_connection.{ActorWithConnection, ActorWithTcp, Outer}
import org.justcards.commons.{ErrorOccurred, LogIn, Logged}
import org.justcards.commons.AppMessage._
import org.justcards.commons.AppError._

object TestConnectionManager {
  def apply(testActor: ActorRef): ConnectionManager = _ => Props(classOf[TestConnectionManager], testActor)

  private[this] class TestConnectionManager(testActor: ActorRef) extends Actor {
    override def receive: Receive = {
      case a => testActor ! a
    }
  }
}

object TestView {
  def apply(testActor: ActorRef): ViewFactory =
    (appController: AppController) => new TestViewImpl(appController, testActor)

  class TestViewImpl(appController: AppController, testActor: ActorRef) extends View {

    override def error(message: String): Unit = testActor ! ErrorOccurred(message)

    override def logged(): Unit = testActor ! Logged()
  }
}

object LogInView {

  def apply(username: String, testActor: ActorRef): ViewFactory =
    (appController: AppController) => new LogInViewImpl(appController, username, testActor)

  private[this] class LogInViewImpl(appController: AppController, username: String, testActor: ActorRef)
    extends TestViewImpl(appController, testActor) with View {
    appController login username
  }
}

trait ConnectionHandler extends (ActorRef => Props)

object Server {
  def apply(serverAddress: InetSocketAddress, connectionHandler: ConnectionHandler) =
    Props(classOf[Server], serverAddress, connectionHandler)

  private[this] class Server(serverAddress: InetSocketAddress, connectionHandler: ConnectionHandler) extends Actor {
    import akka.io.Tcp._
    import context.system

    IO(Tcp) ! Bind(self, serverAddress)

    override def receive: Receive = {
      case b @ Bound(_) =>
        //println("main server: bond")
        context.parent ! b
      case CommandFailed(_: Bind) => println("main server: cmd failed"); context.stop(self)
      case c @ Connected(_, _) =>
        //println("server: "  + c + " has a new client")
        val connection = sender()
        val handler = context.actorOf(connectionHandler(connection))
        connection ! Register(handler)
      case m => println("main server: not handled " + m)
    }
  }
}

object AllowLoginManager extends ConnectionHandler {
  override def apply(connection: ActorRef) = Props(classOf[AllowLoginManagerWithTcp], connection)

  private[this] abstract class AllowLoginManager(connection: ActorRef) extends ActorWithConnection with Actor {

    override def receive: Receive = parse orElse {
      case Outer(LogIn(username)) => connection ==> Logged(username)
      case m => println("allow login: not handled " + m)
    }
  }

  private[this] class AllowLoginManagerWithTcp(connection: ActorRef) extends AllowLoginManager(connection) with ActorWithTcp
}

object DenyLoginManager extends ConnectionHandler {
  override def apply(connection: ActorRef) = Props(classOf[DenyLoginManagerWithTcp], connection)

  private[this] abstract class DenyLoginManager(connection: ActorRef) extends ActorWithConnection with Actor {

    override def receive: Receive = parse orElse {
      case Outer(LogIn(_)) => connection ==> ErrorOccurred(USER_ALREADY_PRESENT)
      case m => println("deny login: not handled " + m)
    }
  }

  private[this] class DenyLoginManagerWithTcp(connection: ActorRef) extends DenyLoginManager(connection) with ActorWithTcp
}
