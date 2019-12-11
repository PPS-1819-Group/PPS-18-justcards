package org.justcards.client.view

import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.Executors

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import org.justcards.client.controller.AppController._
import MenuChoice._
import FilterChoice._
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.commons.games_rules.{PointsConversion, PointsConversionType, Rule}
import org.justcards.commons.games_rules.PointsConversionType._
import org.justcards.commons.games_rules.Rule._
import org.justcards.server.Commons.BriscolaSetting
import org.justcards.server.Commons.BriscolaSetting._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Success


class ConsoleView(controller: ActorRef) extends Actor {
  import View._
  import ConsoleView._

  private implicit val executor: ExecutionContextExecutor =
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
        MenuChoice(numberChoice) match {
          case LIST_LOBBY_WITH_FILTERS =>
            askLobbyFilters()
            context >>> lobbyByFilters
          case JOIN_LOBBY_BY_ID =>
            askLobbyId()
            context >>> lobbyById
          case other => controller ! MenuSelection(other)
        }
      } (askMenuChoice())
    case ShowLobbyCreation(games) =>
      val gamesList = games.toList
      showLobbyCreationOptions(gamesList)
      context >>> lobbyCreation(gamesList)
    case ShowLobbies(lobbies) => context toLobbyLookup lobbies
    case ShowAvailableGames(games) =>
      printGameList(games)
      context >>> gamesList(games)
    case ShowGameCreation(rules, cards, seeds) =>
      val numberCards = cards.map(_.number)
      gameCreation(rules.toList)((numberCards.min, numberCards.max), seeds)
  }

  private def lobbyById: Receive = {
    case NewUserCommand(choice) =>
      if(choice == BACK)
        context toMenu
      else
        controller ! MenuSelection(JOIN_LOBBY_BY_ID, Map(BY_ID -> choice))
    case ShowError(error) if error == LOBBY_NOT_EXISTING =>
      println(errorMessage(error))
      askLobbyId()
    case ShowJoinedLobby(lobby, members) => context joinLobby(lobby, members)
  }

  private def lobbyByFilters: Receive = {
    case NewUserCommand(choice) =>
      if(choice == BACK)
        context toMenu
      else {
        val limit = FilterChoice.values.count(!_.toString.isEmpty)
        parseToNumberAnd(choice, limit) { numberChoice =>
          askToUser(VALUE_CHOICE)
          val filterChoice = FilterChoice(numberChoice)
          context >>> {
            case NewUserCommand(filter) =>
              controller ! MenuSelection(LIST_LOBBY_WITH_FILTERS, Map(filterChoice -> filter))
              context >>> lobbyByFilters
          }
        }(askLobbyFilters())
      }
    case ShowError(error) if error == LOBBY_NOT_EXISTING =>
      println(errorMessage(error))
      askLobbyFilters()
    case ShowLobbies(lobbies) => context toLobbyLookup lobbies
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
    case ShowJoinedLobby(lobby, members) => context joinLobby(lobby, members)
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
    case NewUserCommand(`EXIT`) => controller ! ExitFromLobby
    case NewUserCommand(_) => askToUser("")
    case ShowGameStarted(players) =>
      val myTeam = players.find(_._1.name == myUsername).get._2
      val mate = players find(tuple => tuple._2 == myTeam && tuple._1.name != myUsername) map(_._1)
      clearAndPrint(GAME_STARTED(myTeam, mate))
      printPlayersOrder(myUsername, players)
      context >>> inGame(myTeam)
  }

  private def inGame(myTeam: TeamId, myCards: List[Card] = List(), briscola: (String, Option[Int]) = ("", None)): Receive = {
    case ShowGameWinner(teamWinner) =>
      clearAndPrint(if(teamWinner == myTeam) GAME_WON else GAME_LOST(teamWinner))
    case ShowMatchWinner(winnerTeam, matchPoints, totalPoints) =>
      clearAndPrint(MATCH_RESULTS_TITLE)
      println(MATCH_RESULTS(matchPoints))
      println(MATCH_ENDS(winnerTeam, totalPoints))
    case ShowHandWinner(player) =>
      clearAndPrint(if(player.name == myUsername) HAND_WON else HAND_LOST(player))
    case ShowChosenBriscola(seed, number) =>
      val newBriscola = (seed, number)
      printBriscola(newBriscola)
      context >>> inGame(myTeam, myCards, newBriscola)
    case ShowGameInformation(myCards, fieldCards) =>
      val orderedCards = myCards.ordered
      printGameInformation(orderedCards, fieldCards, briscola, isMyTurn = false)
      context >>> inGame(myTeam, orderedCards, briscola)
    case ShowTurn(myCards, fieldCards, timeout) =>
      askCard(myTeam, myCards.ordered, fieldCards, briscola)(timeout seconds)
    case ViewChooseBriscola(briscolaSet, timeout) => askBriscola(briscolaSet.toList, timeout seconds)(myTeam, myCards)
    case ShowMenu => context toMenu
  }

  private def userDoMove(onUserChoice: String => Unit)(errorToIntercept: AppError)
                        (myTeam: TeamId, myCards: List[Card], briscola: (String, Option[Int]) = ("", None)): Receive = {
    case NewUserCommand(choice) => onUserChoice(choice)
    case ShowTimeForMoveExceeded =>
      println()
      println(TIME_IS_UP)
      context >>> inGame(myTeam, myCards, briscola)
    case MoveWasCorrect => context >>> inGame(myTeam, myCards, briscola)
    case ShowError(`errorToIntercept`) =>
      println(errorMessage(errorToIntercept))
      context >>> inGame(myTeam, myCards, briscola)
  }

  private def gamesList(games: Set[(GameId, Long)]): Receive = {
    case NewUserCommand(_) => askToUser("")
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

  private def waitGameCreation(cardRestrictions: (Int, Int), seeds: Set[String]): Receive = {
    case ShowGameCreated => println(GAME_CREATED_MESSAGE)
    case ShowMenu => context toMenu
    case ShowError(error) if error == GAME_ALREADY_EXISTS =>
      println(errorMessage(error))
      askGameName(Map(), cardRestrictions, seeds)
    case ShowNotValidRules(rules) =>
      println(ERROR_RULES)
      gameCreation(rules.toList)(cardRestrictions, seeds)
  }

  private def gameCreation(rules: List[Rule.Value], completedRules: Map[Rule.Value,Any] = Map())
                          (cardRestrictions: (Int, Int), seeds: Set[String]): Unit = {
    def _onRuleCreated(rule: (Rule.Value, Any)): Unit =
      gameCreation(rules.tail, completedRules + rule)(cardRestrictions, seeds)
    if(rules isEmpty) {
      askGameName(completedRules, cardRestrictions, seeds)
    } else {
      println(LINE_DELIMETER)
      rules.head match {
        case CARDS_DISTRIBUTION =>
          askToUser(CARD_DISTRIBUTION_MESSAGE)
          context >>> cardsDistribution(_onRuleCreated)
        case PLAY_SAME_SEED =>
          askToUser(PLAY_SAME_SEED_MESSAGE)
          context >>> yesOrNoResponse(PLAY_SAME_SEED)(_onRuleCreated)
        case Rule.CHOOSE_BRISCOLA =>
          askBriscolaSettings()
          context >>> {
            case NewUserCommand(choice) => parseToNumberAnd(choice, BriscolaSetting.maxId){
              n => _onRuleCreated((Rule.CHOOSE_BRISCOLA,BriscolaSetting(n - 1)))
            }(askToUser(""))
          }
        case POINTS_TO_WIN_SESSION =>
          askToUser(POINTS_LIMIT_MESSAGE)
          context >>> {
            case NewUserCommand(value) => value toOptionInt match {
              case Some(v) => _onRuleCreated((POINTS_TO_WIN_SESSION, v))
              case _ => askToUser(WRONG_VALUE)
            }
          }
        case POINTS_OBTAINED_IN_A_MATCH =>
          val pointsList = (PointsConversionType.values - EXACTLY).toList
          askPointConversion(POINTS_OBTAINED_MESSAGE, pointsList)
          context >>> pointsConversion(
            POINTS_OBTAINED_IN_A_MATCH,
            pointsList
          )(_onRuleCreated)(askPointConversion(POINTS_OBTAINED_MESSAGE))
        case WINNER_POINTS =>
          askPointConversion(WINNER_POINTS_MESSAGE)
          context >>> pointsConversion(WINNER_POINTS)(_onRuleCreated)(askPointConversion(WINNER_POINTS_MESSAGE))
        case LOSER_POINTS =>
          askPointConversion(LOSER_POINTS_MESSAGE)
          context >>> pointsConversion(LOSER_POINTS)(_onRuleCreated)(askPointConversion(LOSER_POINTS_MESSAGE))
        case DRAW_POINTS =>
          askPointConversion(DRAW_POINTS_MESSAGE)
          context >>> pointsConversion(DRAW_POINTS)(_onRuleCreated)(askPointConversion(DRAW_POINTS_MESSAGE))
        case STARTER_CARD =>
          askToUser(STARTER_CARD_BOOLEAN_MESSAGE)
          context >>> yesOrNoResponse(STARTER_CARD)(wantsStarterCard => {
            if(wantsStarterCard._2){
              askStarterCard(cardRestrictions, seeds)
              context >>> starterCard(seeds, cardRestrictions)(_onRuleCreated)
            } else gameCreation(rules.tail, completedRules)(cardRestrictions, seeds)
          })
        case LAST_TAKE_WORTH_ONE_MORE_POINT =>
          askToUser(LAST_TAKE_MESSAGE)
          context >>> yesOrNoResponse(LAST_TAKE_WORTH_ONE_MORE_POINT)(_onRuleCreated)
        case CARDS_HIERARCHY_AND_POINTS =>
          askCardsHierarchyAndPoints(cardRestrictions)
          context >>> cardsSpecification()(cardRestrictions, cardRestrictions._2)(_onRuleCreated)
        case _ => gameCreation(rules.tail, completedRules)(cardRestrictions, seeds)
      }
    }
  }

  private def cardsSpecification(cardSettings: Map[Int,(Int,Int)] = Map())(cardRestrictions: (Int,Int), remainingCards: Int)
                                (onComplete: ((Rule.Value,CardsHierarchyAndPoints)) => Unit): Receive = {
    case NewUserCommand(value) =>
      val values = value.split("\\|")
      if (values.size == 3)
        (values(0).toOptionInt, values(1).toOptionInt, values(2).toOptionInt) match {
          case (Some(number),_,_) if cardSettings.contains(number) =>
            askToUser(CARD_ALREADY_PRESENT)
          case (Some(number),_,_) if number < cardRestrictions._1 || number > cardRestrictions._2 =>
            askToUser(WRONG_CARD)
          case (Some(number),Some(points),Some(hierarchy)) =>
            val newCardsSettings = cardSettings + (number -> (points, hierarchy))
            if (remainingCards == 1) {
              val cardsHierarchyAndPoints =
                newCardsSettings.map(x => (x._1,x._2._1,x._2._2)).toList.sortWith { (card1, card2) =>
                  card1._3 > card2._3
                }.map(x => (x._1,x._2))
              println(CARDS_ADDED)
              onComplete((Rule.CARDS_HIERARCHY_AND_POINTS, cardsHierarchyAndPoints))
            } else {
              askToUser(NEXT_CARD)
              context >>> cardsSpecification(newCardsSettings)(cardRestrictions, remainingCards - 1)(onComplete)
            }
          case _ => askToUser(WRONG_PATTERN)
        }
      else askToUser(WRONG_PATTERN)
  }

  private def pointsConversion(rule: Rule.Value, points: List[PointsConversionType] = PointsConversionType.values.toList)
                              (onComplete: ((Rule.Value, PointsConversion)) => Unit)(onError: => Unit): Receive = {
    case NewUserCommand(choice) => parseToNumberAnd(choice, points.size)(num =>
      points(num - 1) match {
        case MATCH_POINTS => onComplete((rule, PointsConversion(MATCH_POINTS)))
        case option =>
          askToUser(VALUE_CHOICE)
          context >>> {
            case NewUserCommand(value) => value.toOptionInt match {
              case Some(v) => onComplete((rule,PointsConversion(option,v)))
              case _ => askToUser(WRONG_VALUE)
            }
          }
      }
    )(onError)
  }

  private def cardsDistribution(onComplete: ((Rule.Value,CardsDistribution)) => Unit): Receive = {
    case NewUserCommand(value) =>
      val values = value.split("\\|")
      if (values.size == 3) {
        (values(0).toOptionInt, values(1).toOptionInt, values(2).toOptionInt) match {
          case (Some(h),Some(d),Some(f)) => onComplete((CARDS_DISTRIBUTION,(h,d,f)))
          case _ => askToUser(WRONG_PATTERN)
        }
      } else askToUser(WRONG_PATTERN)
  }

  private def yesOrNoResponse(rule: Rule.Value)(onComplete: ((Rule.Value, Boolean)) => Unit): Receive = {
    case NewUserCommand(value) if value == "y" || value == "yes" => onComplete((rule,true))
    case NewUserCommand(value) if value == "n" || value == "no" => onComplete((rule,false))
    case NewUserCommand(_) => askToUser(WRONG_BOOLEAN)
  }

  private def starterCard(seeds: Set[String], cardsRestrictions: (Int,Int))(onComplete: ((Rule.Value, Card)) => Unit): Receive = {
    case NewUserCommand(value) =>
      val values = value.split("\\|")
      if (values.size == 2) {
        (values(0).toOptionInt, values(1)) match {
          case (None, _) => askToUser(WRONG_CARD)
          case (Some(number), _) if number < cardsRestrictions._1 || number > cardsRestrictions._2 =>
            askToUser(WRONG_CARD)
          case (_, seed) if !(seeds contains seed) => askToUser(WRONG_CARD)
          case (Some(number),seed) => onComplete((STARTER_CARD,Card(number,seed)))
        }
      } else askToUser(WRONG_PATTERN)
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

  private def askLobbyId(): Unit = {
    clearAndPrint(LOBBY_BY_ID_TITLE)
    askToUser(ID_CHOICE)
  }

  private def askLobbyFilters(): Unit = {
    clearAndPrint(LOBBY_BY_FILTER_TITLE)
    for (choice <- FilterChoice.values; if !choice.toString.isEmpty)
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

  private def askCard(myTeam: TeamId, myCards: List[Card], fieldCards: List[Card], briscola: (String, Option[Int]))
                     (timeout: FiniteDuration): Unit = {
    def ask() : Unit = {
      printGameInformation(myCards, fieldCards, briscola, isMyTurn = true)
      askToUser(CHOOSE_CARD(timeout))
    }

    ask()
    context >>> userDoMove { choice =>
      parseToNumberAnd(choice, myCards.size)(numberChoice => {
        controller ! ChosenCard(myCards(numberChoice - 1))
        context >>> inGame(myTeam, myCards, briscola)
      })(ask())
    }(AppError.CARD_NOT_VALID)(myTeam, myCards, briscola)
  }

  private def askCardsHierarchyAndPoints(cards: (Int, Int)): Unit = {
    println(CARD_NUMBER_RANGE + ((cards._1 to cards._2) mkString " | "))
    askToUser(CARD_INSERT_MESSAGE(cards._1, cards._2))
  }

  private def askPointConversion(msg: String, list: List[PointsConversionType] = PointsConversionType.values.toList): Unit = {
    println(msg)
    printNumberAndOption(list)(printPoints)
    askToUser("")
  }

  private def askStarterCard(cards: (Int, Int), seeds: Set[String]): Unit = {
    println(CARD_NUMBER_RANGE + ((cards._1 to cards._2) mkString " | "))
    println(SEEDS + (seeds mkString " | "))
    askToUser(STARTER_CARD_MESSAGE)
  }

  private def askBriscolaSettings(): Unit = {
    println(BRISCOLA_SETTINGS_MESSAGE)
    for (choice <- BriscolaSetting.values)
      println(choice.id + 1 + ")" + printBriscolaSettings(choice))
    askToUser("")
  }

  private def askGameName(completedRules: Map[Rule.Value, Any], cardRestrictions: (Int, Int), seeds: Set[String]): Unit = {
    askToUser(ASK_NAME)
    context >>> {
      case NewUserCommand(name) =>
        controller ! AppControllerCreateGame(name, completedRules)
        context >>> waitGameCreation(cardRestrictions, seeds)
    }
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

  private def printPlayersOrder(myUsername: String, players: List[(UserId, TeamId)]): Unit = {
    val playersToPrint = players map {
      case (UserId(_, `myUsername`), _) => "you"
      case (UserId(_, username), team) => username + " (" + team.name + ")"
    } mkString " -> "
    println(PLAYERS_ORDER + playersToPrint)
  }

  private def printGameInformation(myCards: List[Card], fieldCards: List[Card],
                                   briscola: (String, Option[Int]), isMyTurn: Boolean): Unit = {
    printFieldCards(fieldCards)
    printBriscola(briscola)
    printMyCards(myCards, isMyTurn)
  }

  private def printBriscola(briscola: (String, Option[Int])): Unit = {
    if(briscola.isDefined) {
      val message: String = briscola match {
        case (seed, None) if !seed.isBlank => BRISCOLA_NO_CARD(seed)
        case (seed, number) if !seed.isBlank => BRISCOLA_WITH_CARD(Card(number.get, seed))
        case _ => ""
      }
      println(message)
    }
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

  private def printGameList(games: Set[(GameId, Long)]): Unit = {
    import java.util.Calendar
    import java.util.Locale
    clearAndPrint(GAMES_TITLE)
    implicit val format = new SimpleDateFormat("dd MMMM yyyy", Locale.ENGLISH)
    val calendar = Calendar.getInstance
    println(games map(tuple => {
      calendar.setTimeInMillis(tuple._2)
      printGames(tuple._1.name, calendar.getTime)
    }) mkString(start="- ", sep="\n- ", end=""))
    askToUser(GO_BACK)
  }

  private def goodbye(): Unit = {
    println(LINE_DELIMETER)
    println(GOODBYE)
    println(LINE_DELIMETER)
  }

  private def askToUser(question: String): Unit = {
    if(!awaitingUserChoice) {
      awaitingUserChoice = true
      Future { ask(question) } onSuccessfulComplete { result =>
        import akka.pattern.ask
        import akka.util.Timeout
        implicit val timeout = Timeout(3 seconds)
        awaitingUserChoice = false
        if(result == BACK)
          controller ? CanGoBackToMenu onComplete {
            case Success(GoBackToMenu(true)) => context toMenu
            case _ => self ! NewUserCommand(result)
          }
        else self ! NewUserCommand(result)
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

    def toLobbyLookup(lobbies: Set[(LobbyId, Set[UserId])]): Receive = {
      val lobbiesList = lobbies.toList
      showLobbies(lobbiesList)
      val nextBehaviour = lobbyLookup(lobbiesList)
      context >>> nextBehaviour
      nextBehaviour
    }

    def toLobby: Receive = {
      showLobbyOptions()
      context >>> inLobby
      inLobby
    }

    def joinLobby(lobby:LobbyId, members: Set[UserId]): Unit = {
      clearAndPrint(LOBBY_JOINED_MESSAGE(lobby))
      printLobbyState(lobby, members)
      toLobby
    }
  }

}

object ConsoleView {

  def apply(): View = controller => Props(classOf[ConsoleView], controller)

  private val BACK = "back"
  private val NUMBER_CHOICE = "Insert number of your choice or \"" concat BACK concat "\" to return to the menu:"
  private val ID_CHOICE = "Insert the lobby ID you want to join:"
  private val VALUE_CHOICE = "Insert value:"
  private val WRONG_VALUE = "Error: unacceptable value"
  private val WRONG_BOOLEAN = "Error: only yes/y or no/n are allowed"
  private val BOOLEAN_CHOICE = "(yes/y, no/n)"
  private val EMPTY_RESPONSE = "Empty answer isn't allowed"
  private val LINE_DELIMETER = "---------------------------"
  private val FIELD_CARD_MESSAGE = "Cards on the field: -> "
  private val HAND_CARD_MESSAGE = "Cards on your hand: "
  private val CHOOSE_NICKNAME = "Choose your nickname:"
  private val CHOOSE_BRISCOLA = "Choose the briscola you want"
  private val PLAYERS_ORDER = "Players order: "
  private val GO_BACK = "Write \"" concat BACK concat "\" to return to the menu"
  private val CHOOSE_BRISCOLA_TIMEOUT = (time: FiniteDuration) => CHOOSE_BRISCOLA + ", you have " + time + ":"
  private val CHOOSE_CARD = (time: FiniteDuration) => "Insert the number of the card you want to play, you have " + time + ":"
  private val GOODBYE = "Bye"
  /* --------------------------------------------------- */
  /* game creation messages */
  private val CARD_NUMBER_RANGE = "Card number range -> "
  private val SEEDS = "Available seeds -> "
  private val CARD_ALREADY_PRESENT = "This card is already present, insert a new one!"
  private val WRONG_PATTERN = "You've not followed the pattern (x|y|z), try again"
  private val WRONG_CARD = "This card doesn't exist, try again"
  private val NEXT_CARD = "Next card"
  private val CARDS_ADDED = "All cards added!"
  private val CARD_INSERT_MESSAGE = (min: Int, max:Int) => "Insert for each card in the previous range its number, its value" +
    " and its position in the hierarchy in the form of \n number|value|hierarchy where " + min +
    " is the highest value of hierarchy and " + max + " is the lowest"
  private val CARD_DISTRIBUTION_MESSAGE = "Insert how many cards each player has to have when the game starts," +
    " how many cards you can draw after each hand turn and how many cards will be on the field in the form of \n" +
    "handCards|drawCards|fieldCards"
  private val POINTS_OBTAINED_MESSAGE =
    "Insert the number corresponding to how you want to gain points after a match:"
  private val WINNER_POINTS_MESSAGE =
    "Insert the number corresponding to how many points the match winner will get:"
  private val LOSER_POINTS_MESSAGE =
    "Insert the number corresponding to how many points the match loser will get:"
  private val DRAW_POINTS_MESSAGE =
    "Insert the number corresponding to how many points you'll get after a match in case of a draw:"
  private val PLAY_SAME_SEED_MESSAGE = "Do the players have to play a card with the same seed of the" +
    " one played by the first player in the current hand? " concat BOOLEAN_CHOICE
  private val LAST_TAKE_MESSAGE = "Does the last take of a match have to value one extra point? " concat BOOLEAN_CHOICE
  private val STARTER_CARD_BOOLEAN_MESSAGE =
    "Do you want that a player with a specific card starts the game?" concat BOOLEAN_CHOICE
  private val STARTER_CARD_MESSAGE = "Insert the card that defines who is the first player in the form of \number|seed"
  private val BRISCOLA_SETTINGS_MESSAGE =
    "Insert the number corresponding to how the Briscola will be chosen during a game session"
  private val POINTS_LIMIT_MESSAGE = "Insert the points limit to reach in order to win a session:"
  private val ASK_NAME = "Insert the name of the game"
  private val GAME_CREATED_MESSAGE = "Your game has been successfully created!!!"
  private val ERROR_RULES = "Some of the rules you've created are wrong, change them!"

  def printPoints(points: PointsConversionType): String = points match {
    case EXACTLY => "The amount of points that you indicate"
    case DIVIDE => "The amount of points obtained in the match divided by a value that you indicate"
    case MATCH_POINTS => "The amount of points obtained in the match"
  }

  def printBriscolaSettings(setting: BriscolaSetting): String = setting match {
    case USER => "The first user of each matches will decide Briscola"
    case SYSTEM => "The system will decide the Briscola"
    case NOT_BRISCOLA => "Briscola is not present"
  }

  //my messages
  private case class NewUserCommand(command: String)

  private def clearAndPrint(message: String = ""): Unit = {
    println(LINE_DELIMETER)
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

  private def printGames(gameName: String, date: Date)(implicit format: SimpleDateFormat): String =
    gameName concat " created on " concat format.format(date.getTime)

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
    if(!question.isBlank) println(question)
    readLine(View.INPUT_SYMBOL) match {
      case a if a.isBlank || a.isEmpty => ask(EMPTY_RESPONSE)
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


  view ! ShowUsernameChoice
  view ! ShowMenu
  /*view ! ShowLobbies(Set())
  view ! ShowJoinedLobby(LobbyId(1234, "pippo", GameId("beccaccino")), Set(UserId(1, "io")))
  view ! ShowGameStarted(List(
    (UserId(1, "chiara"), TeamId("team pippe")),
    (UserId(1, "elena"), TeamId("team ciccio")),
    (UserId(1, "me"), TeamId("team pippe")),
    (UserId(1, "giovanna"), TeamId("team ciccio"))
  ))
  view ! ShowGameInformation(myCards, fieldCards)
  view ! ShowMatchWinner(TeamId("team ciccio"), (12,3), (45,23))
  view ! ShowGameWinner(TeamId("team ciccio"))
  view ! ShowGameWinner(TeamId("team pippe"))

  view ! ShowTurn(myCards, fieldCards, 10)
  view ! ShowTimeForMoveExceeded

  view ! ViewChooseBriscola(Set("cuori", "picche"), 3)

  Thread.sleep(5000)

  view ! MoveWasCorrect
  view ! ShowChosenBriscola("spade")

  Thread.sleep(5000)

  view ! ShowGameInformation(myCards, fieldCards)*/


  val (cards,seeds) = Rule.deckCards
  view ! ShowGameCreation(Rule.values, cards, seeds)
}
