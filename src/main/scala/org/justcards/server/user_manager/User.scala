package org.justcards.server.user_manager

import akka.actor.{ActorLogging, ActorRef, Props, Terminated}
import akka.io.Tcp._
import org.justcards.commons._
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions._
import org.justcards.commons.actor_connection.{ActorWithConnection, ActorWithRemotes, ActorWithTcp, Outer}
import org.justcards.server.Commons.UserInfo
import org.justcards.server.user_manager.UserManagerMessage.{LogOutAndExitFromLobby, UserExitFromLobby, UserRemoved}

abstract class BasicUserActor(userRef: ActorRef, userManager: ActorRef) extends ActorWithConnection with ActorLogging {

  import org.justcards.commons.AppError._

  override def receive: Receive = parse orElse completeBehaviour(notLogged)

  private def notLogged: Receive = {
    case Outer(msg: LogIn) => userManager ! msg
    case Outer(_: LogOut) =>
    case Logged(username) =>
      userRef ==> Logged(username)
      this >>> (logged(username), username)
  }

  private def logged(username: String): Receive = {
    case Outer(LogOut(`username`)) =>
      userManager ! LogOut(username)
      context stop self
    case message: LobbyJoined => toLobby(message, username)
    case message: LobbyCreated => toLobby(message, username)
  }

  private def inLobby(username: String, lobby: LobbyId): Receive = {
    case Outer(LogOut(`username`)) =>
      userManager ! LogOutAndExitFromLobby(username, lobby)
      context stop self
    case Outer(OutOfLobby(`lobby`)) => userManager ! UserExitFromLobby(lobby, UserInfo(username, self))
    case UserRemoved(true) =>
      this >>> logged(username)
      userRef ==> OutOfLobby(lobby)
    case UserRemoved(false) => userRef ==> ErrorOccurred(LOBBY_NOT_EXISTING)
    case msg: GameStarted =>
      this >>> inGame(username, lobby, sender())
      userRef ==> msg
  }

  private def inGame(username: String, lobby: LobbyId, sessionManager: ActorRef): Receive = {
    case Outer(_: LogIn) => userRef ==> ErrorOccurred(USER_ALREADY_LOGGED)
    case Outer(_: LogOut) => userRef ==> ErrorOccurred(USER_WRONG_USERNAME)
    case Outer(msg: AppMessage) => sessionManager ! msg
    case msg @ OutOfLobby(`lobby`) =>
      userRef ==> msg
      this >>> logged(username)
  }

  private def commonBehaviour: Receive = {
    case Outer(_: LogIn) => userRef ==> ErrorOccurred(USER_ALREADY_LOGGED)
    case Outer(_: LogOut) => userRef ==> ErrorOccurred(USER_WRONG_USERNAME)
    case Outer(msg) => userManager ! msg
    case msg: ErrorOccurred => userRef ==> msg
    case message: AppMessage => userRef ==> message
  }

  protected def errorBehaviour(username: String): Receive = {
    case msg => log.debug("User -> " + username + " | unhandled message | " + msg)
  }

  private def completeBehaviour(behaviour: Receive, username: String = ""): Receive =
    behaviour orElse commonBehaviour orElse errorBehaviour(username)

  private def >>>(behaviour: Receive, username: String = ""): Unit = {
    this become completeBehaviour(behaviour, username)
  }

  private def toLobby[A <: {val lobby: LobbyId}](msg: A, username: String): Receive = {
    userRef ==> msg.asInstanceOf[AppMessage]
    val nextBehaviour = inLobby(username, msg.lobby)
    this >>> (nextBehaviour, username)
    nextBehaviour
  }

}

object User {

  def apply(userRef: ActorRef, userManager: ActorRef): Props = User(userRef, userManager, REMOTES)

  def apply(userRef: ActorRef, userManager: ActorRef, mode: ActorWithConnectionOptions): Props = mode match {
    case TCP => Props(classOf[UserActorWithTcp], userRef, userManager)
    case REMOTES =>  Props(classOf[UserActorWithRemotes], userRef, userManager)
  }

  private[this] class UserActorWithTcp(private val userRef: ActorRef, private val userManager: ActorRef)
    extends BasicUserActor(userRef, userManager) with ActorWithTcp {

    import org.justcards.commons.actor_connection.ActorWithTcp._

    private[this] val CONNECTION_CLOSED = "Connection with user has been closed"
    private[this] val COMMAND_FAILED = "Write failed, trying to send => "

    override def errorBehaviour(username: String): Receive = {
      case CommandFailed(w: Write) =>
        // O/S buffer was full
        log.debug(COMMAND_FAILED + extractMessage(w.data))
      case _: ConnectionClosed =>
        log.debug(CONNECTION_CLOSED)
        if (!username.isEmpty) self ! Outer(LogOut(username))
        else context stop self
    }

  }

  private[this] class UserActorWithRemotes(private val userRef: ActorRef, private val userManager: ActorRef)
    extends BasicUserActor(userRef, userManager) with ActorWithRemotes {

    private[this] val CONNECTION_CLOSED = "Connection with user has been closed"

    context.watch(userRef)

    override def errorBehaviour(username: String): Receive = {
      case Terminated(`userRef`) =>
        log.debug(CONNECTION_CLOSED)
        if (!username.isEmpty) self ! Outer(LogOut(username))
        else context stop self
    }

  }

}