package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.controller.AppController
import org.justcards.client.view.{View, ViewFactory}
import org.justcards.commons.actor_connection.{ActorWithConnection, ActorWithTcp, Outer}
import org.justcards.commons.{AppMessage, AvailableGames, AvailableLobbies, ErrorOccurred, GameId, LobbyCreated, LobbyId, LobbyJoined, LobbyUpdate, Logged, UserId}
import org.justcards.commons.AppMessage._

object Utils {
  val serverHost = "localhost"
  val username = "username"
  val game = GameId(1,"my-game")
  val lobby = LobbyId(1)
  val user = UserId(1,username)
  val errorMessage = "error"

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
  def menuSelection(choice: String): Unit
  def createLobby(game: GameId): Unit
  def joinLobby(lobby: LobbyId): Unit
}

object TestView {
  def apply(testActor: ActorRef, hasToSendRef: Boolean = false): ViewFactory =
    (appController: AppController) => new TestViewImpl(appController, testActor, hasToSendRef)

  class TestViewImpl(appController: AppController, testActor: ActorRef, hasToSendRef: Boolean) extends View with UserCommandHandler {

    if (hasToSendRef) testActor ! this

    override def error(message: String): Unit = testActor ! ErrorOccurred(message)

    override def logged(): Unit = testActor ! Logged()

    override def showLobbyCreation(games: Set[GameId]): Unit = testActor ! AvailableGames(games)

    override def showLobbyJoin(lobbies: Set[(LobbyId, Set[UserId])]): Unit = testActor ! AvailableLobbies(lobbies)

    override def lobbyCreated(lobby: LobbyId): Unit = testActor ! LobbyCreated(lobby)

    override def lobbyJoined(lobby: LobbyId, members: Set[UserId]): Unit = testActor ! LobbyJoined(lobby, members)

    override def lobbyUpdate(lobby: LobbyId, members: Set[UserId]): Unit = testActor ! LobbyUpdate(lobby,members)


    override def login(username: String): Unit = appController login username

    override def menuSelection(choice: String): Unit = appController menuSelection choice

    override def createLobby(game: GameId): Unit = appController createLobby game

    override def joinLobby(lobby: LobbyId): Unit = appController joinLobby lobby
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
      case b @ Bound(_) => context.parent ! b
      case CommandFailed(_: Bind) => context.stop(self)
      case _ @ Connected(_, _) =>
        val connection = sender()
        val handler = context.actorOf(connectionHandler(connection))
        connection ! Register(handler)
      case m => println("server(" + serverAddress + "): not handled " + m)
    }
  }
}

trait SenderServer {
  def send(msg: AppMessage): Unit
}

object SimpleConnectionHandler {

  def apply(testActor: ActorRef, hasToSendRef: Boolean = false): ConnectionHandler =
    (connection: ActorRef) => Props(classOf[SimpleConnectionHandlerWithTcp], connection, testActor, hasToSendRef)

  private[this] abstract class SimpleConnectionHandlerImpl(connection: ActorRef, testActor: ActorRef, hasToSendRef: Boolean) extends ActorWithConnection with Actor with SenderServer {

    if (hasToSendRef) testActor ! this

    override def receive: Receive = parse orElse {
      case Outer(m) => testActor ! m
      case m => testActor ! m
    }

    override def send(msg: AppMessage): Unit = connection ==> msg

  }

  private[this] class SimpleConnectionHandlerWithTcp(connection: ActorRef, testActor: ActorRef, hasToSendRef: Boolean)
    extends SimpleConnectionHandlerImpl(connection, testActor, hasToSendRef) with ActorWithTcp
}
