package org.justcards.client

trait ConsoleManager extends Thread {
  def chooseNickname(): Unit
  def errorLogin(): Unit
  def showMenuAndChoose(): Unit
}
