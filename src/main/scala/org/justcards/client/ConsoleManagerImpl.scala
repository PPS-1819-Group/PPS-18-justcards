package org.justcards.client

case class ConsoleManagerImpl(controller: AppController) extends ConsoleManager {
  import scala.io.StdIn._

  val TaskAskNickname = Task( () => controller login ask("Choose your nickname:"))
  val TaskErrorLogin = Task( () => {
    println("Error: Nickname already used")
    controller login ask("Choose your nickname:")
  })
  val TaskMenuChoice = Task( () => {
    println("MENU")
    for (choice <- MenuChoice.values) println(choice.id + ")" + choice)
    controller selectionMenu menuChoiceSelection
  })

  override def chooseNickname(): Unit = TaskAskNickname start()

  override def errorLogin(): Unit = TaskErrorLogin start()

  override def showMenuAndChoose(): Unit = TaskMenuChoice start()

  @scala.annotation.tailrec
  private def menuChoiceSelection(): MenuChoice.Value = {
    checkMenuChoice (ask("Insert number of your choice:")) match {
      case Some(choice) => choice
      case None =>
        println("Error: unacceptable value")
        menuChoiceSelection()
    }
  }

  private def checkMenuChoice(response: String): Option[MenuChoice.Value] = {
    try {
      Some(MenuChoice(response.toInt))
    } catch {
      case e: Exception => None
    }

  }

  private def ask(question: String): String = {
    println(question)
    readLine
  }
}

case class Task(exec: () => Unit) extends Thread {
  override def run(): Unit = exec()
}

object TestConsole extends App{
  val console = ConsoleManagerImpl(new AppControllerImpl)
  console showMenuAndChoose()
}
