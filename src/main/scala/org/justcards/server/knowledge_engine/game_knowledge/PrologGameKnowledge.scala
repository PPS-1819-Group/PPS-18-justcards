package org.justcards.server.knowledge_engine.game_knowledge

import java.io.FileInputStream

import alice.tuprolog.{Prolog, SolveInfo, Struct, Term, Theory, Var}
import org.justcards.commons.{Card, GameId}
import org.justcards.server.Commons
import org.justcards.server.Commons.BriscolaSetting
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.Commons.Team.Team
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge._

class PrologGameKnowledge(private val game: GameId) extends GameKnowledge {

  import PrologGameKnowledge._
  import Struct._

  private val defaultCardsInHand = 0
  private val defaultCardsToDraw = 0
  private val defaultCardsOnField = 0

  private val draw = "draw"
  private val hand = "startHand"
  private val card = "card"
  private val briscolaSetting = "chooseBriscola"
  private val currentBriscola = "currentBriscola"
  private val seed = "seed"
  private val variableStart = "VAR"
  private val knowledge: Prolog = createKnowledge(game)
  private val baseTheory = knowledge getTheory

  override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = {
    val variable = getVariables(1) head
    val cardsInHand = knowledge.getFirstSolution(new Struct(hand,variable),variable)(_.toInt)
    val cardsToDraw = knowledge.getFirstSolution(new Struct(draw,variable),variable)(_.toInt)
    (
      cardsInHand getOrElse defaultCardsInHand,
      cardsToDraw getOrElse defaultCardsToDraw,
      defaultCardsOnField
    )
  }

  override def deckCards: Set[Card] = {
    val variables = getVariables(2)
    val number = variables head
    val seed = variables(1)
    val cardsFound = for (solution <- knowledge.getAllSolutions(new Struct(card,number,seed)))
      yield (solution.getOptionValue(number)(_.toInt),solution.getValue(seed)(_.toString))
    cardsFound filter (card => card._1.isDefined && card._2.isDefined) map (card => Card(card._1.get, card._2.get))
  }

  override def hasToChooseBriscola: BriscolaSetting = {
    val setting = getVariables(1).head
    knowledge.getFirstSolution(new Struct(briscolaSetting,setting),setting)(_.toInt) match {
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
      println(knowledge.getTheory)
    }
    briscolaValid
  }

  override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = ???

  override def handWinner(fieldCards: List[(Card, Commons.UserInfo)]): Commons.UserInfo = ???

  override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = ???

  override def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Option[Team] = ???

  override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = ???

  private def getVariables(amount: Int): List[Var] =
    (for (variableNumber <- 0 until amount) yield new Var(variableStart + variableNumber)).toList

  private implicit class RichProlog(knowledge: Prolog) {

    def getFirstSolution[X](goal: Term, variable: Var)(termToOptionX: Term => Option[X]): Option[X] =
      getFirstSolution(goal).getOptionValue(variable)(termToOptionX)

    def getFirstSolution(goal: Term): Map[String,Term] = getSolvedVars(knowledge solve goal)

    def getAllSolutions(goal: Term): Set[Map[String,Term]] = solveAll(knowledge solve goal)()

    def exist(goal: Term): Boolean = knowledge.solve(goal).isSuccess

    @scala.annotation.tailrec
    private[this] final def solveAll(info: SolveInfo)(solutions: Set[Map[String,Term]] = Set()): Set[Map[String,Term]] =
      if (info isSuccess) {
        val newSolutions = solutions + getSolvedVars(info)
        if (knowledge hasOpenAlternatives) solveAll(knowledge solveNext())(newSolutions) else newSolutions
      } else solutions

    private[this] def getSolvedVars(info: SolveInfo): Map[String,Term] = {
      import scala.collection.JavaConverters._
      val solvedVars = for (variable <- info.getBindingVars.asScala) yield (variable.getName, variable.getTerm)
      solvedVars filter (_ != null) toMap
    }
  }

  private implicit class RichTerm(term: Term) {
    import alice.tuprolog.Number
    def toInt: Option[Int] = if (term.isInstanceOf[Number]) Some(term.toString.toInt) else None
  }

  private implicit class RichMap[K,V](map: Map[K,V]) {
    def getOptionValue[X](key: K)(termToOptionX: V => Option[X]): Option[X] = map get key match {
        case Some(value) => termToOptionX(value)
        case _ => None
      }
    def getValue[X](key: K)(termToX: V => X): Option[X] = map get key match {
        case Some(value) => Some(termToX(value))
        case _ => None
      }
  }

  private[this] object Struct {
    def apply(name: String, parameters: Term*): Struct = new Struct(name, parameters.toArray)

    def apply(terms: Term*): Struct = new Struct(terms.toArray)

    def apply(): Struct = new Struct()
    
    implicit def fromStringToTerm(value: String): Term = Term.createTerm(value)
  }

  private[this] object Theory {
    def apply(clauseList: Struct): Theory = new Theory(clauseList)
  }

  private implicit def varToString(variable: Var): String = variable getName

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
