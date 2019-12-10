package org.justcards.server.user_manager

import akka.actor.ActorRef
import akka.testkit.TestProbe
import org.justcards.commons.LogIn

object Utils {

  def doLogIn(userManager: ActorRef, username: String)(implicit me: TestProbe): Unit = {
    implicit val myRef = me.ref
    userManager ! LogIn(username)
    me receiveN 1
  }

}
