package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.view.View
import org.justcards.client.view.View._
import org.justcards.commons.actor_connection.{ActorWithConnection, ActorWithTcp, Outer}
import org.justcards.commons.{AppError, AppMessage, GameId, LobbyId, UserId}

object Utils {
  val serverHost = "localhost"
  val username = "username"
  val game = GameId("my-game")
  val lobby = LobbyId(1,"owner", game)
  val user = UserId(1,username)
  val errorMessage: AppError.Value = AppError.SELECTION_NOT_AVAILABLE
}

object ReSendConnectionManager {
  def apply(testActor: ActorRef): ConnectionManager = _ => Props(classOf[ReSendConnectionManager], testActor)

  private[this] class ReSendConnectionManager(testActor: ActorRef) extends Actor {
    override def receive: Receive = {
      case a => testActor ! a
    }
  }
}

object TestView {

  def apply(testActor: ActorRef): View = _ => Props(classOf[TestViewImpl], testActor)

  class TestViewImpl(testActor: ActorRef) extends Actor {
    override def receive: Receive = {
      case m: ViewMessage => testActor ! m
    }
  }
}

trait ConnectionHandler extends (ActorRef => Props)

object Server {

  case object ServerReady

  def apply(serverAddress: InetSocketAddress, connectionHandler: ConnectionHandler, testActor: ActorRef) =
    Props(classOf[Server], serverAddress, connectionHandler, testActor)

  private[this] class Server(serverAddress: InetSocketAddress, connectionHandler: ConnectionHandler,
                             testActor: ActorRef) extends Actor with ActorLogging {
    import akka.io.Tcp._
    import context.system

    IO(Tcp) ! Bind(self, serverAddress)

    override def receive: Receive = {
      case _ @ Bound(_) => testActor ! ServerReady
      case CommandFailed(_: Bind) => context.stop(self)
      case _ @ Connected(_, _) =>
        val connection = sender()
        val handler = context.actorOf(connectionHandler(connection))
        connection ! Register(handler)
    }
  }
}

object SimpleConnectionHandler {

  def apply(testActor: ActorRef): ConnectionHandler =
    (connection: ActorRef) => Props(classOf[SimpleConnectionHandlerWithTcp], connection, testActor)

  private[this] abstract class SimpleConnectionHandlerImpl(connection: ActorRef, testActor: ActorRef)
    extends ActorWithConnection with Actor with ActorLogging {

    testActor ! self

    override def receive: Receive = parse orElse {
      case Outer(m) => testActor ! m
      case m: AppMessage => connection ==> m
      case m => testActor ! m
    }

  }

  private[this] class SimpleConnectionHandlerWithTcp(connection: ActorRef, testActor: ActorRef)
    extends SimpleConnectionHandlerImpl(connection, testActor) with ActorWithTcp
}

object TestAppController {
  def apply(testActor: ActorRef) = Props(classOf[TestAppController], testActor)
  private[this] class TestAppController(testActor: ActorRef) extends Actor with ActorLogging {
    override def receive: Receive = {
      case m => testActor ! m
    }
  }
}
