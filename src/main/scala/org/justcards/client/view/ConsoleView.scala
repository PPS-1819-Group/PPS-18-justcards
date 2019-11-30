package org.justcards.client.view

import java.util.concurrent.Executors

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import org.justcards.client.controller.AppController._
import org.justcards.commons._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}


class ConsoleView(controller: ActorRef) extends Actor {
  import View._
  import ConsoleView._

  implicit val executor: ExecutionContextExecutor =
    scala.concurrent.ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  private var awaitingUserChoice: Boolean = false
  private var myUsername: String = ""

  override def receive: Receive = init orElse errorManagement

  def init: Receive = {
    case ShowUsernameChoice => askUsername()
    case NewUserCommand(username) =>
      controller ! ChosenUsername(username)
      myUsername = username
    case ShowMenu => context toMenu
  }

  private def inMenu: Receive = {
    case NewUserCommand(choice) => //chosen a menu voice
      parseToNumberAnd(choice, MenuChoice.maxId - 1) { numberChoice =>
        controller ! MenuSelection(numberChoice)
      } (askMenuChoice())
    case ShowLobbyCreation(games) =>
      val gamesList = games.toList
      showLobbyCreationOptions(gamesList)
      context >>> lobbyCreation(gamesList)
    case ShowLobbies(lobbies) =>
      val lobbiesList = lobbies.toList
      showLobbies(lobbiesList)
      context >>> lobbyLookup(lobbiesList)
  }

  private def lobbyLookup(lobbies: List[(LobbyId, Set[UserId])]): Receive = {
    case NewUserCommand(choice) => // chosen a lobby
      parseToNumberAnd(choice, lobbies.size) { numberChoice =>
        controller ! AppControllerJoinLobby(lobbies(numberChoice - 1)._1)
      } (showLobbies(lobbies))
    case ShowLobbies(newLobbies) =>
      val lobbiesList = newLobbies.toList
      showLobbies(lobbiesList)
      context >>> lobbyLookup(lobbiesList)
    case ShowJoinedLobby(lobby, members) =>
      clearAndPrint(LOBBY_JOINED_MESSAGE(lobby))
      printLobbyState(lobby, members)
      context toLobby
  }

  private def lobbyCreation(games: List[GameId]): Receive = {
    case NewUserCommand(choice) => //chosen a game for the lobby
      parseToNumberAnd(choice, games.size) { numberChoice =>
        controller ! AppControllerCreateLobby(games(numberChoice - 1))
      } (showLobbyCreationOptions(games))
    case ShowCreatedLobby(lobby) =>
      clearAndPrint(LOBBY_CREATED_MESSAGE(lobby))
      context toLobby
  }

  private def inLobby: Receive = {
    case ShowMenu => context toMenu
    case ShowLobbyUpdate(lobby, members) =>
      println() ; clearAndPrint()
      printLobbyState(lobby, members)
      showLobbyOptions()
    case NewUserCommand(command) => command match {
      case EXIT => controller ! ExitFromLobby
    }
    case ShowGameStarted(team) =>
      clearAndPrint(GAME_STARTED(team))
      context >>> inGame(team)
  }

  private def inGame(myTeam: TeamId, myCards: List[Card] = List()): Receive = {
    case ShowGameWinner(teamWinner) =>
      clearAndPrint(if(teamWinner == myTeam) GAME_WON else GAME_LOST(teamWinner))
    case ShowMatchWinner(winnerTeam, team1Points, team2Points) =>
      clearAndPrint(MATCH_ENDS(winnerTeam, (team1Points, team2Points)))
    case ShowHandWinner(player) =>
      clearAndPrint(if(player.name == myUsername) HAND_WON else HAND_LOST(player))
    case ShowGameInformation(myCards, fieldCards) =>
      val orderedCards = myCards.ordered
      printGameInformation(orderedCards, fieldCards, isMyTurn = false)
      context >>> inGame(myTeam, orderedCards)
    case ShowTurn(myCards, fieldCards, timeout) => askCard(myTeam, myCards.ordered, fieldCards, timeout seconds)
    case ViewChooseBriscola(briscolaSet, timeout) => askBriscola(briscolaSet.toList, timeout seconds)(myTeam, myCards)
    case ShowMenu => context toMenu
  }

  private def userDoMove(onUserChoice: String => Unit)(errorToIntercept: AppError.Value)
                        (myTeam: TeamId, myCards: List[Card]): Receive = {
    case NewUserCommand(choice) => onUserChoice(choice)
    case ShowTimeForMoveExceeded =>
      println(TIME_IS_UP)
      context >>> inGame(myTeam, myCards)
    case MoveWasCorrect => context >>> inGame(myTeam, myCards)
    case ShowError(`errorToIntercept`) =>
      println(errorMessage(errorToIntercept))
      context >>> inGame(myTeam, myCards)
  }

  private def disconnected: Receive = {
    case NewUserCommand(command) => //chosen what to do
      parseToNumberAnd(command, OptionConnectionFailed.maxId - 1) { numberChoice =>
        val option = fromIntToOptionConnectionFailed(numberChoice)
        controller ! ReconnectOption(option)
        if(option == OptionConnectionFailed.QUIT) goodbye()
        context >>> init
      } (showReconnectOptions())
  }

  private def errorManagement: Receive = {
    case ShowError(error) =>
      println(errorMessage(error))
      if(error == AppError.CANNOT_CONNECT) {
        showReconnectOptions()
        context >>> disconnected
      }
  }

  private def askUsername(): Unit = askToUser(CHOOSE_NICKNAME)

  private def askMenuChoice(): Unit = {
    clearAndPrint(MENU_TITLE)
    for (choice <- MenuChoice.values)
      println(choice.id + ")" + choice)
    askToUser(NUMBER_CHOICE)
  }

  private def askBriscola(briscolaList: List[String], timeout: FiniteDuration)(myTeam: TeamId, myCards: List[Card]): Unit = {
    def ask(showTimeout: Boolean = false): Unit = {
      println(briscolaList mkString " | ")
      askToUser(if(showTimeout) CHOOSE_BRISCOLA_TIMEOUT(timeout) else CHOOSE_BRISCOLA)
    }

    ask(showTimeout = true)
    context >>> userDoMove (choice => {
      val briscola = briscolaList find(_.toLowerCase == choice.toLowerCase)
      if(briscola isDefined) controller ! ChosenBriscola(briscola.get)
      else {
        println(WRONG_VALUE)
        ask()
      }
    })(AppError.BRISCOLA_NOT_VALID)(myTeam, myCards)
  }

  private def askCard(myTeam: TeamId, myCards: List[Card], fieldCards: List[Card], timeout: FiniteDuration): Unit = {
    def ask() : Unit = {
      printGameInformation(myCards, fieldCards, isMyTurn = true)
      askToUser(CHOOSE_CARD(timeout))
    }

    ask()
    context >>> userDoMove { choice =>
      parseToNumberAnd(choice, myCards.size)(numberChoice => {
        controller ! ChosenCard(myCards(numberChoice - 1))
        context >>> inGame(myTeam, myCards)
      })(ask())
    }(AppError.CARD_NOT_VALID)(myTeam, myCards)
  }

  private def showLobbyCreationOptions(games: List[GameId]): Unit = {
    clearAndPrint(LOBBY_CREATION_TITLE)
    printNumberAndOption(games)(_.name)
    askToUser(NUMBER_CHOICE)
  }

  private def showLobbies(lobbies: List[(LobbyId, Set[UserId])]): Unit = {
    clearAndPrint(LOBBY_LIST_TITLE)
    if(lobbies isEmpty) println(DEFAULT_LOBBIES_MESSAGE)
    else
      for (
        index <- 1 to lobbies.size;
        lobby = lobbies(index - 1)
      ) println(index + ")" + "[" + lobby._2.size + "/4 players]" + fromLobbyToString(lobby._1))
    askToUser(NUMBER_CHOICE)
  }

  private def showLobbyOptions(): Unit = askToUser(LOBBY_MESSAGE)

  private def showReconnectOptions(): Unit = {
    for (option <- OptionConnectionFailed.values)
      println (option.id + ")" + option)
    askToUser(NUMBER_CHOICE)
  }

  private def printLobbyState(lobby: LobbyId, players: Set[UserId]): Unit = {
    println(lobby + ", players (" + players.size + "/4):")
    println(players map fromUserToString mkString(start = "- ", sep = "\n- ", end = ""))
  }

  private def printGameInformation(myCards: List[Card], fieldCards: List[Card], isMyTurn: Boolean): Unit = {
    printFieldCards(fieldCards)
    printMyCards(myCards, isMyTurn)
  }

  private def printFieldCards(cards: List[Card]): Unit = {
    clearAndPrint()
    printCards(FIELD_CARD_MESSAGE, cards)
  }

  private def printMyCards(cards: List[Card], isMyTurn: Boolean): Unit =  {
    if(isMyTurn) {
      println(HAND_CARD_MESSAGE)
      printNumberAndOption(cards)(fromCardToString)
    } else {
      printCards(HAND_CARD_MESSAGE, cards)
    }
  }

  private def goodbye(): Unit = {
    println(NEW_LINE)
    println(GOODBYE)
    println(NEW_LINE)
  }

  private def askToUser(question: String): Unit = {
    if(!awaitingUserChoice) {
      awaitingUserChoice = true
      Future { ask(question) } onSuccessfulComplete {
        awaitingUserChoice = false
        self ! NewUserCommand(_)
      }
    } else {
      println(question)
      print(View.INPUT_SYMBOL)
    }
  }

  private implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = {
      require(behaviour != errorManagement)
      context become (behaviour orElse errorManagement)
    }

    def toMenu: Receive = {
      askMenuChoice()
      context >>> inMenu
      inMenu
    }

    def toLobby: Receive = {
      showLobbyOptions()
      context >>> inLobby
      inLobby
    }
  }

}

object ConsoleView {

  def apply(): View = controller => Props(classOf[ConsoleView], controller)

  private val NUMBER_CHOICE = "Insert number of your choice:"
  private val WRONG_VALUE = "Error: unacceptable value"
  private val EMPTY_RESPONSE = "Empty answer isn't allowed"
  private val NEW_LINE = "---------------------------"
  private val FIELD_CARD_MESSAGE = "Cards on the field: => "
  private val HAND_CARD_MESSAGE = "Cards on your hand: "
  private val CHOOSE_NICKNAME = "Choose your nickname:"
  private val CHOOSE_BRISCOLA = "Choose the briscola you want"
  private val CHOOSE_BRISCOLA_TIMEOUT = (time: FiniteDuration) => CHOOSE_BRISCOLA + ", you have " + time + ":"
  private val CHOOSE_CARD = (time: FiniteDuration) => "Insert the number of the card you want to play, you have " + time + ":"
  private val GOODBYE = "Bye"

  //my messages
  private case class NewUserCommand(command: String)

  private def clearAndPrint(message: String = ""): Unit = {
    println(NEW_LINE)
    if(!message.isEmpty) println(message)
  }

  private def printCards(message: String, cards: List[Card]): Unit = {
    print(message)
    println(cards map fromCardToString mkString " | ")
  }

  private def printNumberAndOption[A](list: List[A])(extractor: A => String): Unit = {
    for (index <- 1 to list.size)
      println(index + ")" + extractor(list(index - 1)))
  }

  private def parseToNumberAnd(choice: String, size: Int)(okThen: Int => Unit)(orElse: => Unit): Unit = {
    val numberChoice = parseNumberChoice(choice, size)
    if(numberChoice isDefined) okThen(numberChoice.get)
    else {
      println(WRONG_VALUE)
      orElse
    }
  }

  private def parseNumberChoice(choice: String, maxValue: Int): Option[Int] = {
    try {
      choice toInt match {
        case a if 0 < a && a <= maxValue => Some(a)
      }
    } catch {
      case _ : Exception => None
    }
  }

  @scala.annotation.tailrec
  private def ask(question: String): String = {
    import scala.io.StdIn._
    println(question)
    print(View.INPUT_SYMBOL)
    readLine match {
      case a if a.isBlank || a.isEmpty =>
        println(EMPTY_RESPONSE)
        ask(question)
      case a => a
    }
  }

}

object TestConsole extends App {

  import akka.actor._
  import View._

  class Pippo extends Actor {
    override def receive: Receive = {
      case m => println(m)
    }
  }

  val actorSystem = ActorSystem("view-test")
  val ooo = actorSystem.actorOf(Props(classOf[Pippo]))
  val view = actorSystem.actorOf(View()(ooo))

  val myCards = Set(Card(2, "denara"), Card(1, "denara"), Card(1, "coppe"), Card(2, "coppe"), Card(3, "denara"), Card(3, "coppe"))
  val fieldCards = List(Card(3, "coppe"))

  view ! ShowMenu
  view ! ShowLobbies(Set())
  view ! ShowJoinedLobby(LobbyId(1234, "pippo", GameId("beccaccino")), Set(UserId(1, "io")))
  view ! ShowGameStarted(TeamId("team pippe"))
  view ! ShowGameInformation(myCards, fieldCards)
  view ! ShowMatchWinner(TeamId("team ciccio"), 12, 45)
  view ! ShowGameWinner(TeamId("team ciccio"))
  view ! ShowGameWinner(TeamId("team pippe"))

  view ! ShowTurn(myCards, fieldCards, 10)
  view ! ShowTimeForMoveExceeded

  //view ! ViewChooseBriscola(Set("cuori", "picche"), 3)
}
