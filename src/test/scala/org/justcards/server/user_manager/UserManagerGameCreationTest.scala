package org.justcards.server.user_manager

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.TestProbe
import org.justcards.commons._
import org.justcards.server.Commons.CreateGame
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import org.justcards.server.user_manager.Utils._

class UserManagerGameCreationTest extends WordSpecLike with Matchers with BeforeAndAfterAll {
  private implicit val system: ActorSystem = ActorSystem("UserManagerGameCreationTest")

  private val username = "username"
  private val gameName = "mygame"
  private val createGame = CreateGame(gameName,Map())

  override def afterAll: Unit = {
    system terminate()
  }

  "The user manager" when {

    "is at runtime" should {

      "allow to create a game" in {
        implicit val user = TestProbe()
        implicit val userRef = user.ref
        val gameCreated = GameCreated(GameId(gameName))
        val knowledgeEngine = system.actorOf(PermissiveKnowledgeEngine(gameCreated))
        val userManager = system.actorOf(UserManager(TestProbe().ref, knowledgeEngine))
        doLogIn(userManager,username)
        userManager ! createGame
        user expectMsg gameCreated
      }

      "send an error if a game cannot be created" in {
        implicit val user = TestProbe()
        implicit val userRef = user.ref
        val error = ErrorOccurred(AppError.CANNOT_CREATE_GAME)
        val knowledgeEngine = system.actorOf(PermissiveKnowledgeEngine(error))
        val userManager = system.actorOf(UserManager(TestProbe().ref, knowledgeEngine))
        doLogIn(userManager,username)
        userManager ! createGame
        user expectMsg error
      }

    }

  }
}

object PermissiveKnowledgeEngine {
  def apply(createGameResponse: AppMessage) = Props(classOf[PermissiveKnowledgeEngineImpl],createGameResponse)
  class PermissiveKnowledgeEngineImpl(createGameResponse: AppMessage) extends Actor {
    override def receive: Receive = {
      case CreateGame(name,_) => sender() ! createGameResponse
    }
  }
}
