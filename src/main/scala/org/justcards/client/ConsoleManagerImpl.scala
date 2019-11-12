package org.justcards.client
import org.justcards.commons.{GameId, LobbyId}


case class ConsoleManagerImpl(controller: AppController) extends ConsoleManager {
  import ConsoleManagerImpl._
  import ConsoleManager._


  override def chooseNickname(): Unit = createTaskAskNickname start()

  override def errorLogin(): Unit = createTaskErrorLogin start()

  override def showMenu(): Unit = createTaskMenuChoice start()

  override def showLobbyCreation(games: Set[GameId]): Unit = createTaskLobbyCreation(games)  start()

  override def showLobbyJoining(lobbies: List[Lobby]): Unit = createTaskLobbyJoining(lobbies) start()

  private def createTaskAskNickname = Task( () => controller login ask(CHOOSE_NICKNAME))

  private def createTaskErrorLogin = Task( () => {
    println(NICKNAME_ERROR)
    controller login ask(CHOOSE_NICKNAME)
  })

  private def createTaskMenuChoice = Task( () => {
    println(MENU_TITLE)
    for (choice <- MenuChoice.values) println(choice.id + ")" + choice)
    controller menuSelection choiceSelection(MenuChoice.maxId)
  })

  private def createTaskLobbyCreation(games: Set[GameId]): Task = Task( () => {
    println(LOBBY_CREATION_TITLE)
    val gamesList = games.toList
    for (index <- 1 to gamesList.size) println(index + ")" + gamesList(index-1).name)
    controller createLobby gamesList(choiceSelection(games.size) - 1)
  })

  private def createTaskLobbyJoining(lobbies: List[Lobby]): Task = Task( () => {
    println(LOBBY_LIST_TITLE)
    for (index <- 1 to lobbies.size) println(index + ")" + lobbies(index-1))
    controller joinToLobby lobbies(choiceSelection(lobbies.size) - 1)
  })

}

object ConsoleManagerImpl {
  val NUMBER_CHOICE = "Insert number of your choice:"
  val WRONG_VALUE = "Error: unacceptable value"
  val EMPTY_RESPONSE = "Empty answer isn't allowed"

  private case class Task(exec: () => Unit) extends Thread {
    override def run(): Unit = exec()
  }

  @scala.annotation.tailrec
  private def choiceSelection(maxValue: Int): Int = {
    try {
      ask(NUMBER_CHOICE) toInt match {
        case a if 0 < a && a <= maxValue => a
      }
    } catch {
      case e: Exception =>
        println(WRONG_VALUE)
        choiceSelection(maxValue)
    }
  }

  @scala.annotation.tailrec
  private def ask(question: String): String = {
    import scala.io.StdIn._
    println(question)
    readLine match {
      case a if a.isBlank || a.isEmpty =>
        println(EMPTY_RESPONSE)
        ask(question)
      case a => a
    }
  }

  @throws(classOf[Exception])
  implicit private def IntToMenuChoice(choice: Int): MenuChoice.Value = MenuChoice(choice)

}

object TestConsole extends App{
  val console = ConsoleManagerImpl(new AppControllerImpl)

  //console chooseNickname()
  //console errorLogin()
  console showMenu()
  //console showLobbyCreation (Set( GameId(10, "Beccaccino"), GameId(20, "Briscola")))
  //console showLobbyJoining(List( Lobby(LobbyId(100), "prima", 1), Lobby(LobbyId(200), "seconda", 4) ))
}
