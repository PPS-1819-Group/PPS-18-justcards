package org.justcards.client.view

import java.util.concurrent.Executors

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import org.justcards.client.controller.AppController._
import org.justcards.commons.{AppError, GameId, LobbyId, UserId}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Success


class ConsoleView(controller: ActorRef) extends Actor {
  import View._
  import ConsoleView._

  implicit val executor: ExecutionContextExecutor =
    scala.concurrent.ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  var awaitingUserChoice: Boolean = false

  override def receive: Receive = init orElse errorManagement

  def init: Receive = {
    case ShowUsernameChoice => askUsername()
    case NewUserCommand(username) => controller ! ChosenUsername(username)
    case ShowMenu =>
      askMenuChoice()
      context >>> inMenu
  }

  private def inMenu: Receive = {
    case NewUserCommand(choice) => //chosen a menu voice
      parseToNumberAnd(choice, MenuChoice.maxId - 1) { numberChoice =>
        controller ! MenuSelection(MenuChoice(numberChoice))
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
      parseToNumberAnd(choice, lobbies.size) { numberChoice =>
        controller ! AppControllerJoinLobby(lobbies(numberChoice - 1)._1)
      } (showLobbies(lobbies))
    case ShowLobbies(newLobbies) =>
      val lobbiesList = newLobbies.toList
      showLobbies(lobbiesList)
      context >>> lobbyLookup(lobbiesList)
    case ShowJoinedLobby(lobby, members) =>
      clearAndPrint(LOBBY_JOINED_MESSAGE(lobby))
      printLobbyState(lobby, members)
      showLobbyOptions()
      context >>> inLobby
  }

  private def lobbyCreation(games: List[GameId]): Receive = {
    case NewUserCommand(choice) => //chosen a game for the lobby
      parseToNumberAnd(choice, games.size) { numberChoice =>
        controller ! AppControllerCreateLobby(games(numberChoice - 1))
      } (showLobbyCreationOptions(games))
    case ShowCreatedLobby(lobby) =>
      clearAndPrint(LOBBY_CREATED_MESSAGE(lobby))
      showLobbyOptions()
      context >>> inLobby
  }

  private def inLobby: Receive = {
    case ShowLobbyUpdate(lobby, members) =>
      println() ; println(NEW_LINE)
      printLobbyState(lobby, members)
      showLobbyOptions()
    case NewUserCommand(command) => command match {
      case EXIT => controller ! ExitFromLobby
    }
  }

  private def disconnected: Receive = {
    case NewUserCommand(command) => //chosen what to do
      parseToNumberAnd(command, OptionConnectionFailed.maxId - 1) { numberChoice =>
        val option = OptionConnectionFailed(numberChoice)
        controller ! ReconnectOption(option)
        if(option == OptionConnectionFailed.QUIT) goodbye()
        context >>> init
      } (showReconnectOptions())
  }

  private def errorManagement: Receive = {
    case ShowError(error) =>
      println(errorMessage(error))
      if(error == AppError.CANNOT_CONNECT) {
        showReconnectOptions()
        context >>> disconnected
      }
  }

  private def askUsername(): Unit = askToUser(CHOOSE_NICKNAME)

  private def askMenuChoice(): Unit = {
    clearAndPrint(MENU_TITLE)
    for (choice <- MenuChoice.values)
      println(choice.id + ")" + choice)
    askToUser(NUMBER_CHOICE)
  }

  private def showLobbyCreationOptions(games: List[GameId]): Unit = {
    clearAndPrint(LOBBY_CREATION_TITLE)
    for (index <- 1 to games.size)
      println(index + ")" + games(index - 1).name)
    askToUser(NUMBER_CHOICE)
  }

  private def showLobbies(lobbies: List[(LobbyId, Set[UserId])]): Unit = {
    clearAndPrint(LOBBY_LIST_TITLE)
    for (
      index <- 1 to lobbies.size;
      lobby = lobbies(index - 1)
    ) println(index + ")" + "[" + lobby._2.size + "/4 players]" + fromLobbyToString(lobby._1))
    askToUser(NUMBER_CHOICE)
  }

  private def showLobbyOptions(): Unit = askToUser(LOBBY_MESSAGE)

  private def showReconnectOptions(): Unit = {
    for (option <- OptionConnectionFailed.values)
      println (option.id + ")" + option)
    askToUser(NUMBER_CHOICE)
  }

  private def printLobbyState(lobby: LobbyId, players: Set[UserId]): Unit = {
    println(lobby + ", players (" + players.size + "/4):")
    for(player <- players)
      println("- " + player.name)
  }

  private def goodbye(): Unit = {
    println(NEW_LINE)
    println(GOODBYE)
    println(NEW_LINE)
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
      print(View.INPUT_SYMBOL)
    }
  }

  private def parseToNumberAnd(choice: String, size: Int)(okThen: => Int => Unit)(orElse: => Unit): Unit = {
    val numberChoice = parseNumberChoice(choice, size)
    if(numberChoice isDefined) okThen(numberChoice.get)
    else {
      println(WRONG_VALUE)
      orElse
    }
  }

  private implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = {
      require(behaviour != errorManagement)
      context become (behaviour orElse errorManagement)
    }
  }

}

object ConsoleView {

  def apply(): View = controller => Props(classOf[ConsoleView], controller)

  private val NUMBER_CHOICE = "Insert number of your choice:"
  private val WRONG_VALUE = "Error: unacceptable value"
  private val EMPTY_RESPONSE = "Empty answer isn't allowed"
  private val NEW_LINE = "---------------------------"
  private val GOODBYE = "Bye"

  //my messages
  private case class NewUserCommand(command: String)

  private def clearAndPrint(message: String): Unit = {
    println(NEW_LINE)
    println(message)
  }

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
    print(View.INPUT_SYMBOL)
    readLine match {
      case a if a.isBlank || a.isEmpty =>
        println(EMPTY_RESPONSE)
        ask(question)
      case a => a
    }
  }

  implicit private def fromLobbyToString(lobby: LobbyId): String =
    "Lobby <" + lobby.id + "> created by " + lobby.owner + " | Game: " + lobby.game.name

  @throws(classOf[Exception])
  implicit private def IntToMenuChoice(choice: Int): MenuChoice.Value = MenuChoice(choice)

  @throws(classOf[Exception])
  implicit private def IntToOptionConnectionFailed(choice: Int): OptionConnectionFailed.Value = OptionConnectionFailed(choice)

  implicit class RichFuture[A](future: Future[A]) {
    def onSuccessfulComplete(nextOperation: => A => Unit)(implicit executor: ExecutionContext): Unit =
      future onComplete {
        case Success(value) => nextOperation(value)
        case _ =>
      }
  }

}

object TestConsole extends App {
}
