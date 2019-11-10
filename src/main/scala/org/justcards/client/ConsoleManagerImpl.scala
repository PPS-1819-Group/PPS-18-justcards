package org.justcards.client

case class ConsoleManagerImpl(controller: AppController) extends ConsoleManager {
  import scala.io.StdIn._

  val TaskAskNickname = Task( () => controller login ask("Choose your nickname:"))
  val TaskErrorLogin = Task( () => {
    println("Error: Nickname already used")
    controller login ask("Choose your nickname:")
  })

  override def chooseNickname(): Unit = TaskAskNickname start()

  override def errorLogin(): Unit = TaskErrorLogin start()

  override def showMenuAndChoose(): Unit = ???


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
  console chooseNickname()
}
