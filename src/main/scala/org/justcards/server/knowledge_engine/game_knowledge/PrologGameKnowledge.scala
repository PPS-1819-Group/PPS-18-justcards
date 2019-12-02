package org.justcards.server.knowledge_engine.game_knowledge

import java.io.FileInputStream

import alice.tuprolog.{Prolog, SolveInfo, Struct, Term, Theory, Var}
import org.justcards.commons.{Card, GameId}
import org.justcards.server.Commons.{BriscolaSetting, Team, UserInfo}
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.Commons.Team.Team
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge._

class PrologGameKnowledge(private val game: GameId) extends GameKnowledge {

  import PrologGameKnowledge._
  import PrologUtils._

  private val defaultCardsInHand = 0
  private val defaultCardsToDraw = 0
  private val defaultCardsOnField = 0
  private val defaultPoints = 0

  private val draw = "draw"
  private val hand = "startHand"
  private val card = "card"
  private val briscolaSetting = "chooseBriscola"
  private val currentBriscola = "currentBriscola"
  private val seed = "seed"
  private val turn = "turn"
  private val fieldWinner = "fieldWinner"
  private val matchWinner = "matchWinner"
  private val totalPoints = "totalPoints"
  private val sessionWinner = "sessionWinner"
  private val variableStart = "VAR"
  private val knowledge: Prolog = createKnowledge(game)
  private val baseTheory = knowledge getTheory

  override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = {
    val variable = variables() head
    val cardsInHand = knowledge.firstSolution(new Struct(hand,variable),variable)(_.toInt)
    val cardsToDraw = knowledge.firstSolution(new Struct(draw,variable),variable)(_.toInt)
    (cardsInHand getOrElse defaultCardsInHand, cardsToDraw getOrElse defaultCardsToDraw, defaultCardsOnField)
  }

  override def deckCards: Set[Card] = {
    val vars = variables(amount = 2)
    val number = vars head
    val seed = vars(1)
    val cardsFound = for (solution <- knowledge.allSolutions(Struct(card,number,seed)))
      yield (solution.valueAsOption(number)(_.toInt),solution.valueAs(seed)(_.toString))
    cardsFound filter (card => card._1.isDefined && card._2.isDefined) map (card => Card(card._1.get, card._2.get))
  }

  override def hasToChooseBriscola: BriscolaSetting = {
    val setting = variables().head
    knowledge.firstSolution(new Struct(briscolaSetting,setting),setting)(_.toInt) match {
      case Some(value) if value == 1 => BriscolaSetting.USER
      case Some(value) if value == 0 => BriscolaSetting.SYSTEM
      case _ => BriscolaSetting.NOT_BRISCOLA
    }
  }

  override def setBriscola(seed: Seed): Boolean = {
    val briscolaValid = knowledge exist Struct(this.seed, seed)
    if (briscolaValid) {
      val newCurrentBriscola = Struct(currentBriscola,seed)
      knowledge setTheory baseTheory
      knowledge addTheory Theory(Struct(newCurrentBriscola,Struct()))
    }
    briscolaValid
  }

  override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = {
    val newField = variables().head
    knowledge.firstSolution(Struct(turn, card.toTerm, fieldCards, handCards, newField), newField)(_.toList) match {
      case Some(list) => Some(list map(_.toCard) filter(_.isDefined) map(_.get))
      case _ => None
    }
  }

  override def handWinner(fieldCards: List[(Card, UserInfo)]): UserInfo = {
    val winner = variables() head
    val players = fieldCards map(_._2)
    knowledge.firstSolution(
      Struct(fieldWinner, fieldCards map(v => TupleTerm(v._1.number,v._1.seed,v._2.username)), winner),
      winner
    )(t => Some(t.toString)) match {
      case Some(player) => players find(_.username == player) orNull
      case _ => null
    }
  }

  override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = {
    val vars = variables(amount = 3)
    val teamWinner = vars head
    val firstTeamPoints = vars(1)
    val secondTeamPoints = vars(2)
    knowledge.firstSolution(
      Struct(matchWinner,
        firstTeamCards, secondTeamCards, lastHandWinner.id,
        teamWinner, firstTeamPoints, secondTeamPoints
      )
    ) match {
      case Some(solution) =>
        val winner = solution.valueAsOption(teamWinner)(_.toTeam)
        val firstTeamGainedPoints = solution.valueAsOption(firstTeamPoints)(_.toInt)
        val secondTeamGainedPoints = solution.valueAsOption(secondTeamPoints)(_.toInt)
        (winner orNull, firstTeamGainedPoints getOrElse defaultPoints, secondTeamGainedPoints getOrElse defaultPoints)
      case _ => null
    }
  }

  override def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Option[Team] = {
    val winner = variables().head
    knowledge.firstSolution(Struct(sessionWinner,firstTeamPoints,secondTeamPoints, winner),winner)(_.toTeam)
  }

  override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = {
    val vars = variables(amount = 2)
    val firstTeamPoints = vars head
    val secondTeamPoints = vars(1)
    knowledge.firstSolution(
      Struct(totalPoints, firstTeamCards, secondTeamCards, lastHandWinner id, firstTeamPoints, secondTeamPoints)
    ) match {
      case Some(solution) =>
        val firstTeamGainedPoints = solution.valueAsOption(firstTeamPoints)(_.toInt)
        val secondTeamGainedPoints = solution.valueAsOption(secondTeamPoints)(_.toInt)
        (firstTeamGainedPoints getOrElse defaultPoints, secondTeamGainedPoints getOrElse defaultPoints)
      case _ => null
    }
  }

  private def variables(amount: Int = 1): List[Var] =
    (for (variableNumber <- 0 until amount) yield new Var(variableStart + variableNumber)).toList

  private implicit class RichProlog(knowledge: Prolog) {

    def firstSolution[X](goal: Term, variable: Var)(termToOptionX: Term => Option[X]): Option[X] = firstSolution(goal) match {
        case Some(solution) => solution.valueAsOption(variable)(termToOptionX)
        case _ => None
      }

    def firstSolution(goal: Term): Option[Map[String,Term]] = knowledge solve goal match {
        case info if info.isSuccess => Some(solvedVars(info))
        case _ => None
      }

    def allSolutions(goal: Term): Set[Map[String,Term]] = solveAll(knowledge solve goal)()

    def exist(goal: Term): Boolean = knowledge solve goal isSuccess

    @scala.annotation.tailrec
    private[this] final def solveAll(info: SolveInfo)(solutions: Set[Map[String,Term]] = Set()): Set[Map[String,Term]] =
      if (info isSuccess) {
        val newSolutions = solutions + solvedVars(info)
        if (knowledge hasOpenAlternatives) solveAll(knowledge solveNext())(newSolutions) else newSolutions
      } else solutions

    private[this] def solvedVars(info: SolveInfo): Map[String,Term] = {
      import scala.collection.JavaConverters._
      val solvedVars = for (v <- info.getBindingVars.asScala) yield (v getName, v getTerm)
      solvedVars filter (_ != null) toMap
    }
  }

  private implicit class RichTerm(term: Term) {
    import alice.tuprolog.Number
    import scala.collection.JavaConverters._
    def toInt: Option[Int] = if (term.isInstanceOf[Number]) Some(term.toString.toInt) else None
    def toList: Option[List[Term]] =
      if (term.isList) {
        val list = term.asInstanceOf[Struct]
        Some(list.listIterator().asScala.toList)
      } else None
    def toCard: Option[Card] =
      if (term.isCompound) {
        val card = term.asInstanceOf[Struct]
        card.getArg(0).toString.toOptionInt match {
          case Some(number) => Some(Card(number, card getArg 1 toString))
          case _ => None
        }
      } else None
    def toTeam: Option[Team] = term toInt match {
        case Some(id) => Some(Team(id))
        case _ => None
      }
  }

  private implicit class RichString(value: String) {
    def toOptionInt: Option[Int] =
      try {
        Some(value.toInt)
      } catch {
        case _: Exception => None
      }
  }

  private implicit class RichMap[K,V](map: Map[K,V]) {
    def valueAsOption[X](key: K)(termToOptionX: V => Option[X]): Option[X] = map get key match {
        case Some(value) => termToOptionX(value)
        case _ => None
      }
    def valueAs[X](key: K)(termToX: V => X): Option[X] = map get key match {
        case Some(value) => Some(termToX(value))
        case _ => None
      }
  }

  private implicit class PrologCard(card: Card) {
    def toTerm: Term = TupleTerm(card number,card seed)
  }

  private[this] object Struct {
    def apply(name: String, parameters: Term*): Struct = new Struct(name, parameters toArray)

    def apply(terms: Term*): Struct = Struct(terms toArray)

    def apply(terms: Array[Term]): Struct = new Struct(terms)

    def apply(): Struct = new Struct()

  }

  private[this] object Theory {
    def apply(clauseList: Struct): Theory = new Theory(clauseList)
  }

  private[this] object TupleTerm {
    private[this] val elementSeparator = ","
    def apply(values: String*): Term = Term createTerm values.mkString(elementSeparator)
  }

  private[this] object PrologUtils {
    implicit def fromStringToTerm(value: String): Term = Term createTerm value
    implicit def fromIntToTerm(value: Int): Term = Term createTerm value
    implicit def fromVarToString(variable: Var): String = variable getName
    implicit def fromCardsToTerm(traversable: Traversable[Card]): Term = Struct(traversable map(_.toTerm) toArray)
    implicit def fromTraversableToPrologList(traversable: Traversable[Term]): Term = Struct(traversable toArray)
  }

  private implicit def fromIntToString(value: Int): String = value toString

}

object PrologGameKnowledge {

  def apply(): GameKnowledgeFactory = (game: GameId) => new PrologGameKnowledge(game)

  private[this] val COMMON_RULES_FILE: FileInputStream = GameKnowledge.COMMON_RULES_PATH
  private[this] val COMMON_RULES = new Theory(COMMON_RULES_FILE)

  private[this] implicit def fromStringToInputStream(path: String): FileInputStream = new FileInputStream(path)

  private def createKnowledge(game: GameId): Prolog = {
    val engine = new Prolog()
    val gameTheoryFile: FileInputStream = GAMES_PATH concat game.name.toLowerCase concat ".pl"
    val gameTheory = new Theory(gameTheoryFile)
    engine setTheory COMMON_RULES
    engine addTheory gameTheory
    engine
  }

}
