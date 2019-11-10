package org.justcards.client.connection_manager

import akka.actor.{ActorRef, Props}

trait ConnectionManager extends (ActorRef => Props)
