package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.view.View
import org.justcards.commons.{AppError, Card, GameId, LobbyId, TeamId, UserId}

import scala.concurrent.duration._

object Utils {
  val username: String = "username"
  val game: GameId = GameId("my-game")
  val lobby: LobbyId = LobbyId(1,"owner", game)
  val user: UserId = UserId(1,username)
  val team: TeamId = TeamId("test-team")
  val card: Card = Card(1, "spade")
  val handCards: Set[Card] = Set(Card(1, "spade"), Card(2, "spade"), Card(3, "spade"), Card(5, "coppe"))
  val fieldCards: List[Card] = List(Card(1, "denara"), Card(10, "bastoni"), Card(6, "coppe"))
  val errorMessage: AppError.Value = AppError.SELECTION_NOT_AVAILABLE
  val briscolaTime: FiniteDuration = 2 seconds
  val turnTime: FiniteDuration = 2 seconds

  implicit def fromFiniteDurationToInt(time: FiniteDuration): Int = time._1.toInt
}

object TestConnectionManager {
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
