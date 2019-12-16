package org.justcards.client.controller

import java.util.Calendar

import akka.actor.{Actor, ActorContext, ActorLogging, Props, Timers}

import scala.concurrent.duration._
import scala.reflect.ClassTag
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.client.connection_manager.ConnectionManager.{Connected, ConnectionManagerFactory, DetailedErrorOccurred, InitializeConnection, TerminateConnection}
import org.justcards.client.view.View._
import org.justcards.client.view.MenuChoice._
import org.justcards.client.view.FilterChoice._
import org.justcards.client.view.OptionConnectionFailed._
import org.justcards.commons.games_rules.BriscolaSetting.BriscolaSetting
import org.justcards.commons.games_rules.{PointsConversion, Rule}
import org.justcards.commons.games_rules.Rule._
import org.justcards.commons.games_rules.converter.GameRulesConverter

/**
 * An AppController that encapsulate the client application logic.
 */
object AppController {

  /**
   * Create an AppController.
   * @param connectionManager the ConnectionManagerFactory
   * @param view the ViewFactory
   * @return
   */
  def apply(connectionManager: ConnectionManagerFactory, view: ViewFactory) =
    Props(classOf[AppControllerActor], connectionManager, view)

  case class ChosenUsername(username: String)
  case class ChosenBriscola(briscola: String)
  case class ChosenCard(card: Card)
  case class MenuSelection(choice: MenuChoice, filters: Map[FilterChoice,String] = Map())
  case class AppControllerCreateLobby(game: GameId)
  case class AppControllerJoinLobby(lobby: LobbyId)
  case class ReconnectOption(option: OptionConnectionFailed)
  case class GoBackToMenu(allowed: Boolean)
  case class AppControllerCreateGame(name: String, rules: Map[Rule.Value,Any])
  case object ExitFromLobby
  case object CanGoBackToMenu
}

private[this] class AppControllerActor(connectionManager: ConnectionManagerFactory, view: ViewFactory) extends Actor
              with Timers with ActorLogging {

  import AppController._
  import AppControllerActor._

  private val connectionManagerActor = context actorOf connectionManager(self)
  private val viewActor = context actorOf view(self)
  private val rulesConverter = GameRulesConverter()
  connectionManagerActor ! InitializeConnection

  override def receive: Receive = waitToBeConnected orElse default

  private def waitToBeConnected: Receive = {
    case Connected =>
      context >>> connected
      viewActor ! ShowUsernameChoice
    case ErrorOccurred(m) if m == CANNOT_CONNECT.toString =>
      viewActor ! ShowError(CANNOT_CONNECT)
  }

  private def connected: Receive = {
    case ChosenUsername(username) => connectionManagerActor ! LogIn(username)
    case Logged(_) => context toLogged
  }

  private def logged: Receive = {
    case MenuSelection(choice, filters) => choice match {
      case CREATE_LOBBY =>
        context >>> lobbyCreation
        connectionManagerActor ! RetrieveAvailableGames()
      case LIST_LOBBY =>
        context >>> searchForLobby
        connectionManagerActor ! RetrieveAvailableLobbies()
      case LIST_LOBBY_WITH_FILTERS =>
        context >>> searchForLobby
        connectionManagerActor ! RetrieveAvailableLobbies(
          filters.getOrElse(BY_GAME, ""),
          filters.getOrElse(BY_OWNER, "")
        )
      case JOIN_LOBBY_BY_ID =>
        val lobbyId = toLong(filters(BY_ID))
        if(lobbyId.isEmpty) viewActor ! ShowError(LOBBY_NOT_EXISTING)
        else {
          connectionManagerActor ! JoinLobby(LobbyId(lobbyId.get))
          context >>> waitForLobbyJoined
        }
      case CREATE_GAME => context toGameCreation
      case SHOW_GAMES =>
        connectionManagerActor ! RetrieveAvailableGames()
        context >>> showGames
      case _ => viewActor ! ShowError(SELECTION_NOT_AVAILABLE)
    }
  }

  private def gameCreation(correctRules: Map[Rule.Value, Any] = Map()): Receive = {
    case CanGoBackToMenu => context returnToLogged()
    case AppControllerCreateGame(name,_) if name.isBlank =>
      viewActor ! ShowError(GAME_EMPTY_NAME)
    case AppControllerCreateGame(_,rules) if !(Rule.mandatoryRules subsetOf (rules ++ correctRules).keySet) =>
      viewActor ! ShowError(GAME_MISSING_RULES)
    case AppControllerCreateGame(name,rules) =>
      val wrongRules = nonValidRules(rules)
      if (wrongRules isEmpty) {
        connectionManagerActor ! CreateGame(name, rulesConverter.serialize(rules ++ correctRules))
        context >>> {
          case GameCreated(_) =>
            viewActor ! ShowGameCreated
            context toLogged
          case ErrorOccurred(message) if message.isGameError =>
            val error = AppError.values.find(_.toString == message)
            if(error isDefined){
              viewActor ! ShowError(error.get)
              context toGameCreation correctRules
            }
        }
      } else {
        viewActor ! ShowError(GAME_RULES_NOT_VALID)
        viewActor ! ShowNotValidRules(wrongRules)
        context >>> gameCreation(rules -- wrongRules)
      }
  }

  private def nonValidRules(rules: Map[Rule.Value, Any]): Set[Rule.Value] = rules filterNot {
    case (CARDS_DISTRIBUTION,(h:Int,d:Int,f:Int)) => CARDS_DISTRIBUTION isAllowed (h,d,f)
    case (PLAY_SAME_SEED,v: Boolean) => PLAY_SAME_SEED isAllowed v
    case (CHOOSE_BRISCOLA,v: BriscolaSetting) => CHOOSE_BRISCOLA isAllowed v
    case (POINTS_TO_WIN_SESSION,v :Int) => POINTS_TO_WIN_SESSION isAllowed v
    case (POINTS_OBTAINED_IN_A_MATCH,v: PointsConversion) => POINTS_OBTAINED_IN_A_MATCH isAllowed v
    case (WINNER_POINTS,v: PointsConversion) => WINNER_POINTS isAllowed v
    case (LOSER_POINTS,v: PointsConversion) => LOSER_POINTS isAllowed v
    case (DRAW_POINTS,v: PointsConversion) => DRAW_POINTS isAllowed v
    case (STARTER_CARD,v: Card) => STARTER_CARD isAllowed v
    case (LAST_TAKE_WORTH_ONE_MORE_POINT,v: Boolean) => LAST_TAKE_WORTH_ONE_MORE_POINT isAllowed v
    case (CARDS_HIERARCHY_AND_POINTS,v: List[Any]) =>
      CARDS_HIERARCHY_AND_POINTS isAllowed (v collect { case (number: Int, points: Int) => (number, points) })
    case msg => log.debug("unexpected rule: " + msg); false
  } keySet

  private def lobbyCreation: Receive = {
    case CanGoBackToMenu => context returnToLogged()
    case AvailableGames(games) => viewActor ! ShowLobbyCreation(games map (_._1))
    case AppControllerCreateLobby(game) =>
      connectionManagerActor ! CreateLobby(game)
      context >>> {
        case LobbyCreated(lobby) =>
          context >>> inLobby(lobby)
          viewActor ! ShowCreatedLobby(lobby)
      }
  }

  private def searchForLobby: Receive = {
    case CanGoBackToMenu => context returnToLogged()
    case AvailableLobbies(lobbies) =>
      viewActor ! ShowLobbies(lobbies)
    case AppControllerJoinLobby(lobby) =>
      connectionManagerActor ! JoinLobby(lobby)
      context >>> waitForLobbyJoined
  }

  private def waitForLobbyJoined: Receive = {
    case LobbyJoined(lobby, members) =>
      context >>> inLobby(lobby)
      viewActor ! ShowJoinedLobby(lobby,members)
  }

  private def inLobby(myLobby: LobbyId): Receive = {
    case LobbyUpdate(lobby, members) => viewActor ! ShowLobbyUpdate(lobby,members)
    case ExitFromLobby => connectionManagerActor ! OutOfLobby(myLobby)
    case OutOfLobby(`myLobby`) => context toLogged
    case GameStarted(players) =>
      context >>> inGame
      viewActor ! ShowGameStarted(players)
  }

  private def inGame: Receive = {
    case Information(handCards, fieldCards) => viewActor ! ShowGameInformation(handCards, fieldCards)
    case HandWinner(winner) => viewActor ! ShowHandWinner(winner)
    case MatchWinner(team, matchPoints, totalPoints) => viewActor ! ShowMatchWinner(team, matchPoints, totalPoints)
    case GameWinner(team) => viewActor ! ShowGameWinner(team)
    case CorrectBriscola(seed, number) => viewActor ! ShowChosenBriscola(seed, number)
    case ChooseBriscola(availableBriscola, timeout) => context toChooseBriscola(availableBriscola, timeout)
    case Turn(handCards, fieldCards, timeout) => context toMyTurn((handCards, fieldCards),timeout)
    case OutOfLobby(_) => context toLogged
  }

  private def chooseBriscola(availableBriscola: Set[String], remainingTime: Long, timerStartedTime: Long): Receive =
    userChoose[ChosenBriscola](remainingTime, timerStartedTime){(msg, newRemainingTime) =>
      connectionManagerActor ! Briscola(msg.briscola)
      context >>> waitForBriscolaResponse(availableBriscola, newRemainingTime)
    }

  private def myTurn(cards: (Set[Card], List[Card]), remainingTime: Long, timerStartedTime: Long): Receive =
    userChoose[ChosenCard](remainingTime, timerStartedTime){(msg, newRemainingTime) =>
      connectionManagerActor ! Play(msg.card)
      context >>> waitForMove(cards, newRemainingTime)
    }

  private def waitForBriscolaResponse(availableBriscola: Set[String], remainingTime: Long): Receive =
    waitForChoiceCorrectness[CorrectBriscola](BRISCOLA_NOT_VALID){ briscola =>
      viewActor ! MoveWasCorrect
      viewActor ! ShowChosenBriscola(briscola.seed, briscola.number)
    }(context toChooseBriscola(availableBriscola,remainingTime))

  private def waitForMove(cards: (Set[Card], List[Card]), remainingTime: Long): Receive =
    waitForChoiceCorrectness[Played](CARD_NOT_VALID){ _ =>
      viewActor ! MoveWasCorrect
    }(context toMyTurn(cards,remainingTime))

  private def waitForChoiceCorrectness[A: ClassTag](error: AppError)(onSuccess: A => Unit)(onError: => Unit): Receive = {
    case msg: A =>
      if(timers isTimerActive TimerTimeoutId) timers cancel TimerTimeoutId
      onSuccess(msg)
      context >>> inGame
    case ErrorOccurred(message) if message == error.toString =>
      viewActor ! ShowError(error)
      onError
  }

  private def userChoose[A: ClassTag](remainingTime: Long, timerStartedTime: Long)(onUserChoice: (A,Long) => Unit): Receive = {
    case msg: A =>
      timers cancel TimerTimeoutId
      val timeElapsed = (Calendar.getInstance() - timerStartedTime).millis.toSeconds
      onUserChoice(msg, remainingTime - timeElapsed)
    case Timeout => context timeout()
  }

  private def showGames: Receive = {
    case CanGoBackToMenu => context returnToLogged()
    case AvailableGames(games) => viewActor ! ShowAvailableGames(games)
  }

  private def default: Receive = {
    case CanGoBackToMenu => sender() ! GoBackToMenu(false)
    case ErrorOccurred(message) =>
      val error = AppError.values.find(_.toString == message)
      if (error.isDefined) error.get match {
        case CONNECTION_LOST => connectionLost()
        case MESSAGE_SENDING_FAILED => connectionManagerActor ! TerminateConnection
        case USER_ALREADY_PRESENT => chooseNicknameAgain(USER_ALREADY_PRESENT)
        case USER_NOT_LOGGED => chooseNicknameAgain(USER_NOT_LOGGED)
        case m => notifyError(m)
      }
    case DetailedErrorOccurred(MESSAGE_SENDING_FAILED, message) =>
      connectionManagerActor ! message
    case ReconnectOption(option) => option match {
      case QUIT =>
        context.system.terminate()
        System.exit(1)
      case TRY_TO_RECONNECT =>
        connectionManagerActor ! InitializeConnection
        context >>> waitToBeConnected
    }
  }

  private def connectionLost(): Unit = {
    context >>> waitToBeConnected
    viewActor ! ShowError(CONNECTION_LOST)
    connectionManagerActor ! InitializeConnection
  }

  private def notifyError(message: AppError): Unit = viewActor ! ShowError(message)

  private def chooseNicknameAgain(error: AppError): Unit = {
    viewActor ! ShowError(error)
    viewActor ! ShowUsernameChoice
  }

  private implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = {
      require(behaviour != default)
      context become (behaviour orElse default)
    }

    def timeout(): Unit = {
      context >>> inGame
      viewActor ! ShowTimeForMoveExceeded
      connectionManagerActor ! TimeoutExceeded()
    }

    def returnToLogged(): Unit = {
      sender() ! GoBackToMenu(true)
      context >>> logged
    }

    def toLogged: Receive = {
      val loggedBehaviour = logged
      context >>> loggedBehaviour
      viewActor ! ShowMenu
      loggedBehaviour
    }

    def toChooseBriscola(availableBriscola: Set[String], timeout: Long): Receive = {
      val nextBehaviour = chooseBriscola(availableBriscola, timeout, Calendar.getInstance)
      context >>> nextBehaviour
      viewActor ! ViewChooseBriscola(availableBriscola, timeout.toInt)
      timers startSingleTimer(TimerTimeoutId, Timeout, timeout seconds)
      nextBehaviour
    }

    def toMyTurn(cards: (Set[Card], List[Card]), timeout: Long): Receive = {
      val nextBehaviour = myTurn(cards, timeout, Calendar.getInstance)
      context >>> nextBehaviour
      viewActor ! ShowTurn(cards._1, cards._2, timeout.toInt)
      timers startSingleTimer(TimerTimeoutId, Timeout, timeout seconds)
      nextBehaviour
    }

    def toGameCreation: Receive = {
      val (cards,seeds) = Rule.deckCards
      viewActor ! ShowGameCreation(Rule.values, cards, seeds)
      context >>> gameCreation()
      gameCreation()
    }
  }

}

private[this] object AppControllerActor {
  private implicit def fromCalendarToLong(calendar: Calendar): Long = calendar.getTimeInMillis

  private case object TimerTimeoutId
  private case object Timeout

  private def toLong(value: String): Option[Long] =
    try {
      Some(value.toLong)
    } catch {
      case _: NumberFormatException => None
    }
}