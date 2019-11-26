package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.view.View
import org.justcards.commons.{AppError, GameId, LobbyId, UserId}

object Utils {
  val username = "username"
  val game = GameId("my-game")
  val lobby = LobbyId(1,"owner", game)
  val user = UserId(1,username)
  val errorMessage: AppError.Value = AppError.SELECTION_NOT_AVAILABLE
}

object ReSendConnectionManager {
  def apply(testActor: ActorRef): ConnectionManager = _ => EchoActor(testActor)
}

object TestView {
  def apply(testActor: ActorRef): View = _ => EchoActor(testActor)
}

object EchoActor {
  def apply(testActor: ActorRef) = Props(classOf[EchoActor], testActor)
  private[this] class EchoActor(testActor: ActorRef) extends Actor with ActorLogging {
    override def receive: Receive = {
      case m => testActor ! m
    }
  }
}

object Server {

  case object ServerReady

  def apply(serverAddress: InetSocketAddress, testActor: ActorRef = null, hasToDieAfterConnection: Boolean = false) =
    Props(classOf[Server], serverAddress, testActor, hasToDieAfterConnection)

  private[this] class Server(serverAddress: InetSocketAddress, testActor: ActorRef, hasToDieAfterConnection: Boolean) extends Actor with ActorLogging {
    import akka.io.Tcp._
    import context.system

    IO(Tcp) ! Bind(self, serverAddress)

    override def receive: Receive = {
      case CommandFailed(_: Bind) => context stop self
      case _: Bound if testActor != null => testActor ! ServerReady
      case _: Connected if hasToDieAfterConnection => sender() ! Close
    }
  }
}
