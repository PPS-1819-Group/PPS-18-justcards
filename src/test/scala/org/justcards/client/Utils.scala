package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.controller.AppController
import org.justcards.client.view.{MenuChoice, View, ViewFactory}
import org.justcards.commons.actor_connection.{ActorWithConnection, ActorWithTcp, Outer}
import org.justcards.commons.{AppError, AppMessage, AvailableGames, AvailableLobbies, ErrorOccurred, GameId, LobbyCreated, LobbyId, LobbyJoined, LobbyUpdate, Logged, UserId}

object Utils {
  val serverHost = "localhost"
  val username = "username"
  val game = GameId("my-game")
  val lobby = LobbyId(1,"owner", game)
  val user = UserId(1,username)
  val errorMessage: String = AppError.SELECTION_NOT_AVAILABLE

  def getRef[X](receiveN: Int => Seq[AnyRef]): X = {
    receiveN(1).head.asInstanceOf[X]
  }
}

object ReSendConnectionManager {
  def apply(testActor: ActorRef): ConnectionManager = _ => Props(classOf[ReSendConnectionManager], testActor)

  private[this] class ReSendConnectionManager(testActor: ActorRef) extends Actor {
    override def receive: Receive = {
      case a => testActor ! a
    }
  }
}

trait UserCommandHandler {
  def login(username: String): Unit
  def menuSelection(choice: MenuChoice.Value): Unit
  def createLobby(game: GameId): Unit
  def joinLobby(lobby: LobbyId): Unit
}

object TestView {

  case object ChooseNickname

  def apply(testActor: ActorRef, hasToSendRef: Boolean = false): ViewFactory =
    (appController: AppController) => new TestViewImpl(appController, testActor, hasToSendRef)

  class TestViewImpl(appController: AppController, testActor: ActorRef, hasToSendRef: Boolean) extends View with UserCommandHandler {

    if (hasToSendRef) testActor ! this

    override def error(message: AppError.Value): Unit = testActor ! ErrorOccurred(message)

    override def showMenu(): Unit = testActor ! Logged()

    override def showLobbyCreation(games: Set[GameId]): Unit = testActor ! AvailableGames(games)

    override def showLobbyJoin(lobbies: Set[(LobbyId, Set[UserId])]): Unit = testActor ! AvailableLobbies(lobbies)

    override def lobbyCreated(lobby: LobbyId): Unit = testActor ! LobbyCreated(lobby)

    override def lobbyJoined(lobby: LobbyId, members: Set[UserId]): Unit = testActor ! LobbyJoined(lobby, members)

    override def lobbyUpdate(lobby: LobbyId, members: Set[UserId]): Unit = testActor ! LobbyUpdate(lobby,members)

    override def login(username: String): Unit = appController login username

    override def menuSelection(choice: MenuChoice.Value): Unit = appController menuSelection choice

    override def createLobby(game: GameId): Unit = appController createLobby game

    override def joinLobby(lobby: LobbyId): Unit = appController joinLobby lobby

    override def chooseNickname(): Unit = testActor ! ChooseNickname
  }
}

trait ConnectionHandler extends (ActorRef => Props)

object Server {

  case object ServerReady

  def apply(serverAddress: InetSocketAddress, connectionHandler: ConnectionHandler, testActor: ActorRef) =
    Props(classOf[Server], serverAddress, connectionHandler, testActor)

  private[this] class Server(serverAddress: InetSocketAddress, connectionHandler: ConnectionHandler, testActor: ActorRef) extends Actor with ActorLogging {
    import akka.io.Tcp._
    import context.system

    IO(Tcp) ! Bind(self, serverAddress)
    log.debug("I'm the server and I am " + self)

    override def receive: Receive = {
      case b @ Bound(_) => testActor ! ServerReady
      case CommandFailed(_: Bind) => context.stop(self)
      case _ @ Connected(_, _) =>
        val connection = sender()
        val handler = context.actorOf(connectionHandler(connection))
        connection ! Register(handler)
      case m => log.debug("not handled " + m)
    }
  }
}

object SimpleConnectionHandler {

  def apply(testActor: ActorRef): ConnectionHandler =
    (connection: ActorRef) => Props(classOf[SimpleConnectionHandlerWithTcp], connection, testActor)

  private[this] abstract class SimpleConnectionHandlerImpl(connection: ActorRef, testActor: ActorRef)
    extends ActorWithConnection with Actor with ActorLogging {

    log.debug("I'm the connectionHandler and I am " + self)
    testActor ! self

    override def receive: Receive = parse orElse {
      case Outer(m) =>
        log.debug("from outside " + m)
        testActor ! m
      case m: AppMessage =>
        log.debug("Received message " + m)
        log.debug("Sending to the client")
        connection ==> m
      case m =>
        log.debug("received unhandled message " + m)
        testActor ! m
    }

  }

  private[this] class SimpleConnectionHandlerWithTcp(connection: ActorRef, testActor: ActorRef)
    extends SimpleConnectionHandlerImpl(connection, testActor) with ActorWithTcp
}

object TestAppController {
  def apply(testActor: ActorRef) = Props(classOf[TestAppController], testActor)
  private[this] class TestAppController(testActor: ActorRef) extends Actor with ActorLogging {
    override def receive: Receive = {
      case m =>
        log.debug("received message " + m)
        testActor ! m
    }
  }
}
