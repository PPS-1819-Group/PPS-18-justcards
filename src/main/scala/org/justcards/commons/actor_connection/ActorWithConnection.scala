package org.justcards.commons.actor_connection

import akka.actor.Actor
import akka.actor.Actor.Receive


abstract class ActorWithConnection extends Actor {
  def parse: Receive
  final def become(behaviour: Receive): Unit = context become (parse orElse behaviour)
}
