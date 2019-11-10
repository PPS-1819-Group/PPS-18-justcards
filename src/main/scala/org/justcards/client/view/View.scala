package org.justcards.client.view

import org.justcards.client.controller.AppController

trait View {
  def error(message: String): Unit
  def logged(): Unit
}

/*trait ViewFactory {
  def apply(appController: AppController): View
}*/

trait ViewFactory extends (AppController => View)

object View {
  def apply(): View = new Pippo

  class Pippo extends View {
    override def error(message: String): Unit = ???

    override def logged(): Unit = ???
  }
}