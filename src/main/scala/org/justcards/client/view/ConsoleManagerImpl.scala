package org.justcards.client.view

import java.util.concurrent.Executors

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import org.justcards.commons.{AppError, GameId, LobbyId, UserId}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Success


class ConsoleManagerImpl(controller: ActorRef) extends Actor {
  import View._
  import ConsoleManagerImpl._

  implicit val executor: ExecutionContextExecutor =
    scala.concurrent.ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  var awaitingUserChoice: Boolean = false

  override def receive: Receive = init orElse errorManagement

  def init: Receive = {
    case ChooseNickname => askNickname()
    case NewUserCommand(nickname) => //chose nickname
    //TODO tell the controller the nickname
    case ShowMenu =>
      askMenuChoice()
      context >>> inMenu
  }

  private def inMenu: Receive = {
    case NewUserCommand(choice) => //chosen a menu voice
      parseToNumberAnd(choice, MenuChoice.maxId - 1) {
        //TODO tell the controller the menu choice
      } (askMenuChoice())
    case ShowLobbyCreation(games) =>
      val gamesList = games.toList
      showLobbyCreationOptions(gamesList)
      context >>> lobbyCreation(gamesList)
    case ShowLobbies(lobbies) =>
      val lobbiesList = lobbies.toList
      showLobbies(lobbiesList)
      context >>> lobbyLookup(lobbiesList)
  }

  private def lobbyLookup(lobbies: List[(LobbyId, Set[UserId])]): Receive = {
    case NewUserCommand(choice) => // chosen a lobby
      parseToNumberAnd(choice, lobbies.size) {
        //TODO tell the controller the lobby chosen
      } (showLobbies(lobbies))
    case ShowLobbies(newLobbies) =>
      val lobbiesList = newLobbies.toList
      showLobbies(lobbiesList)
      context >>> lobbyLookup(lobbiesList)
    case LobbyJoined(lobby, members) =>
      println(LOBBY_JOINED_MESSAGE(lobby))
      printLobbyState(lobby, members)
      showLobbyOptions()
      context >>> inLobby
  }

  private def lobbyCreation(games: List[GameId]): Receive = {
    case NewUserCommand(choice) =>
      parseToNumberAnd(choice, games.size) {
        //TODO tell the controller the game chosen
      } (showLobbyCreationOptions(games))
    case LobbyCreated(lobby) =>
      println(LOBBY_CREATED_MESSAGE(lobby))
      showLobbyOptions()
      context >>> inLobby
  }

  private def inLobby: Receive = {
    case LobbyUpdate(lobby, members) => printLobbyState(lobby, members)
    case NewUserCommand(command) => command match {
      case EXIT => //TODO tell the controller to exit from lobby
    }
  }

  private def errorManagement: Receive = {
    case ShowError(error) =>
      println(errorMessage(error))
      if(error == AppError.CANNOT_CONNECT) {
        for (option <- OptionConnectionFailed.values)
          println (option.id + ")" + option)
        /*
        TODO tell the controller the choice
        controller reconnectOrExit choiceSelection(OptionConnectionFailed.maxId - 1)
         */
      }
  }

  private def nextBehaviour(behaviour: Receive) : Receive = behaviour orElse errorManagement

  private def askNickname(): Unit = askToUser(CHOOSE_NICKNAME)

  private def askMenuChoice(): Unit = {
    println(MENU_TITLE)
    for (choice <- MenuChoice.values)
      println(choice.id + ")" + choice)
    askToUser(NUMBER_CHOICE)
  }

  private def showLobbyCreationOptions(games: List[GameId]): Unit = {
    println(LOBBY_CREATION_TITLE)
    for (index <- 1 to games.size)
      println(index + ")" + games(index - 1).name)
    askToUser(NUMBER_CHOICE)
  }

  private def showLobbies(lobbies: List[(LobbyId, Set[UserId])]): Unit = {
    println(LOBBY_LIST_TITLE)
    for (
      index <- 1 to lobbies.size;
      lobby = lobbies(index - 1)
    ) println(index + ")" + lobby._1 + "(" + lobby._2.size + "/4)")
    askToUser(NUMBER_CHOICE)
  }

  private def showLobbyOptions(): Unit = askToUser(LOBBY_MESSAGE)

  private def printLobbyState(lobby: LobbyId, players: Set[UserId]): Unit = {
    println(lobby + ", players (" + players.size + "/4):")
    for(player <- players)
      println("- " + player.name)
  }

  private def askToUser(question: String): Unit = {
    if(!awaitingUserChoice) {
      awaitingUserChoice = true
      Future { ask(question) } onSuccessfulComplete {
        awaitingUserChoice = false
        self ! NewUserCommand(_)
      }
    } else {
      println(question)
      print("> ")
    }
  }

  private def parseToNumberAnd(choice: String, size: Int)(okThen: => Unit)(orElse: => Unit): Unit = {
    val numberChoice = parseNumberChoice(choice, size)
    if(numberChoice isDefined) okThen
    else {
      println(WRONG_VALUE)
      orElse
    }
  }

  implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = context become nextBehaviour(behaviour)
  }

}

object ConsoleManagerImpl {

  def apply(controller: ActorRef): Props = Props(classOf[ConsoleManagerImpl], controller)

  val NUMBER_CHOICE = "Insert number of your choice:"
  val WRONG_VALUE = "Error: unacceptable value"
  val EMPTY_RESPONSE = "Empty answer isn't allowed"

  //my messages
 private case class NewUserCommand(command: String)

  private def parseNumberChoice(choice: String, maxValue: Int): Option[Int] = {
    try {
      choice toInt match {
        case a if 0 < a && a <= maxValue => Some(a)
      }
    } catch {
      case _ : Exception => None
    }
  }

  @scala.annotation.tailrec
  private def ask(question: String): String = {
    import scala.io.StdIn._
    println(question)
    print("> ")
    readLine match {
      case a if a.isBlank || a.isEmpty =>
        println(EMPTY_RESPONSE)
        ask(question)
      case a => a
    }
  }

  @throws(classOf[Exception])
  implicit private def IntToMenuChoice(choice: Int): MenuChoice.Value = MenuChoice(choice)

  @throws(classOf[Exception])
  implicit private def IntToOptionConnectionFailed(choice: Int): OptionConnectionFailed.Value = OptionConnectionFailed(choice)

  implicit class RichFuture[A](future: Future[A]) {
    def onSuccessfulComplete(nextOperation: (A => Unit))(implicit executor: ExecutionContext): Unit =
      future onComplete {
        case Success(value) => nextOperation(value)
        case _ =>
      }
  }

}

object TestConsole extends App {

  /*KeyboardFocusManager.getCurrentKeyboardFocusManager.addKeyEventDispatcher((keyEvent: KeyEvent) => {
    keyEvent.getID match {
      case KeyEvent.KEY_TYPED =>
        println("prova")
      case KeyEvent.KEY_RELEASED =>
        println("prova")
    }
    false
  })*/
  //scala.io.StdIn.readLine()
  //val console = ConsoleManagerImpl(AppController)

  //console chooseNickname()
  //console errorLogin()
  //console showMenu()
  //console showLobbyCreation (Set( GameId(10, "Beccaccino"), GameId(20, "Briscola")))
  //console showLobbyJoining(List( Lobby(LobbyId(100), "prima", 1), Lobby(LobbyId(200), "seconda", 4) ))
}
