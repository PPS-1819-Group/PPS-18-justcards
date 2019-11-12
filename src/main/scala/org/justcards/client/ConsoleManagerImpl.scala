package org.justcards.client
import org.justcards.commons.{GameId, LobbyId}

case class ConsoleManagerImpl(controller: AppController) extends ConsoleManager {
  import scala.io.StdIn._

  override def chooseNickname(): Unit = TaskFactory.TaskAskNickname start()

  override def errorLogin(): Unit = TaskFactory.TaskErrorLogin start()

  override def showMenu(): Unit = TaskFactory.TaskMenuChoice start()

  override def showLobbyCreation(games: Set[GameId]): Unit = TaskFactory.TaskLobbyCreation(games)  start()

  override def showLobbyJoining(lobbies: List[Lobby]): Unit = TaskFactory.TaskLobbyJoining(lobbies) start()

  object TaskFactory {
    val TaskAskNickname = Task( () => controller login ask("Choose your nickname:"))

    val TaskErrorLogin = Task( () => {
      println("Error: Nickname already used")
      controller login ask("Choose your nickname:")
    })

    val TaskMenuChoice = Task( () => {
      println("MENU")
      for (choice <- MenuChoice.values) println(choice.id + ")" + choice)
      controller menuSelection choiceSelection(MenuChoice.maxId)
    })

    def TaskLobbyCreation(games: Set[GameId]): Task = Task( () => {
      println("LOBBY CREATION")
      val gamesList = games.toList
      for (index <- 1 to gamesList.size) println(index + ")" + gamesList(index-1))
      controller createLobby gamesList(choiceSelection(games.size) - 1)
    })

    def TaskLobbyJoining(lobbies: List[Lobby]): Task = Task( () => {
      println("LOBBY LIST")
      for (index <- 1 to lobbies.size) println(index + ")" + lobbies(index-1))
      controller joinToLobby lobbies(choiceSelection(lobbies.size) - 1)
    })
  }

  private def choiceSelection(maxValue: Int): Int = {
    def errorAndRepeat:Int = {
      println("Error: unacceptable value")
      choiceSelection(maxValue)
    }
    try {
      ask("Insert number of your choice:") toInt match {
        case a if 0 < a && a <= maxValue => a
        case _ => errorAndRepeat
      }
    } catch {
      case e: Exception => errorAndRepeat
    }
  }

  @throws(classOf[Exception])
  implicit private def IntToMenuChoice(choice: Int): MenuChoice.Value = MenuChoice(choice)

  private def ask(question: String): String = {
    println(question)
    readLine
  }

}

case class Task(exec: () => Unit) extends Thread {
  override def run(): Unit = exec()
}


/*object TestConsole extends App{
  val console = ConsoleManagerImpl(new AppControllerImpl)

  console.showLobbyJoining(List( Lobby(LobbyId(100), "prima", 1), Lobby(LobbyId(200), "seconda", 4) ))
}*/
