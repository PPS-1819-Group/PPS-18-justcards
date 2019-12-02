package org.justcards.server.knowledge_engine.game_knowledge

import java.io.FileInputStream

import alice.tuprolog.{Prolog,Term,Struct,Var,Theory}
import org.justcards.commons.{Card, GameId}
import org.justcards.server.Commons.{Team, UserInfo}
import org.justcards.server.Commons.BriscolaSetting._
import org.justcards.server.Commons.Team.Team
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge._

class PrologGameKnowledge(private val game: GameId) extends GameKnowledge {

  import PrologGameKnowledge._
  import TuPrologHelpers._

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

  override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = {
    val variable = variables() head
    val cardsInHand = knowledge.find(PrologStruct(hand,variable),variable)(_.toInt)
    val cardsToDraw = knowledge.find(PrologStruct(draw,variable),variable)(_.toInt)
    (cardsInHand getOrElse defaultCardsInHand, cardsToDraw getOrElse defaultCardsToDraw, defaultCardsOnField)
  }

  override def deckCards: Set[Card] = {
    val vars = variables(amount = 2)
    val number = vars head
    val seed = vars(1)
    val cardsFound = for (solution <- knowledge findAll PrologStruct(card,number,seed))
      yield (solution.valueOf(number)(_.toInt),solution.valueOf(seed)(_.toString))
    cardsFound filter (card => card._1.isDefined && card._2.isDefined) map (card => Card(card._1.get, card._2.get))
  }

  override def hasToChooseBriscola: BriscolaSetting = {
    val setting = variables().head
    knowledge.find(PrologStruct(briscolaSetting,setting),setting)(_.toInt) match {
      case Some(value) if value == 1 => USER
      case Some(value) if value == 0 => SYSTEM
      case _ => NOT_BRISCOLA
    }
  }

  override def setBriscola(seed: Seed): Boolean = {
    val briscolaValid = knowledge ? PrologStruct(this.seed, seed)
    if (briscolaValid) {
      knowledge -- PrologStruct(currentBriscola,variables() head)
      knowledge + PrologStruct(currentBriscola,seed)
    }
    briscolaValid
  }

  override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = {
    val newFieldCards = variables().head
    knowledge.find(PrologStruct(turn, card toTerm, fieldCards, handCards, newFieldCards), newFieldCards)(_.toList) match {
      case Some(list) => Some(list map(_.toCard) filter(_.isDefined) map(_.get))
      case _ => None
    }
  }

  override def handWinner(fieldCards: List[(Card, UserInfo)]): UserInfo = {
    val winner = variables() head
    val players = fieldCards map(_._2)
    knowledge.find(
      PrologStruct(fieldWinner, fieldCards map(v => PrologTuple(v._1.number,v._1.seed,v._2.username)), winner),
      winner
    )(_.toString) match {
      case Some(player) => players find(_.username == player) orNull
      case _ => null
    }
  }

  override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = {
    val vars = variables(amount = 3)
    val teamWinner = vars head
    val firstTeamPoints = vars(1)
    val secondTeamPoints = vars(2)
    knowledge.find(
      PrologStruct(matchWinner,
        firstTeamCards, secondTeamCards, lastHandWinner.id,
        teamWinner, firstTeamPoints, secondTeamPoints
      )
    ) match {
      case Some(solution) =>
        val winner = solution.valueOf(teamWinner)(_.toTeam)
        val firstTeamGainedPoints = solution.valueOf(firstTeamPoints)(_.toInt)
        val secondTeamGainedPoints = solution.valueOf(secondTeamPoints)(_.toInt)
        (winner orNull, firstTeamGainedPoints getOrElse defaultPoints, secondTeamGainedPoints getOrElse defaultPoints)
      case _ => null
    }
  }

  override def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Option[Team] = {
    val winner = variables().head
    knowledge.find(PrologStruct(sessionWinner,firstTeamPoints,secondTeamPoints, winner),winner)(_.toTeam)
  }

  override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = {
    val vars = variables(amount = 2)
    val firstTeamPoints = vars head
    val secondTeamPoints = vars(1)
    knowledge.find(
      PrologStruct(totalPoints, firstTeamCards, secondTeamCards, lastHandWinner id, firstTeamPoints, secondTeamPoints)
    ) match {
      case Some(solution) =>
        val firstTeamGainedPoints = solution.valueOf(firstTeamPoints)(_.toInt)
        val secondTeamGainedPoints = solution.valueOf(secondTeamPoints)(_.toInt)
        (firstTeamGainedPoints getOrElse defaultPoints, secondTeamGainedPoints getOrElse defaultPoints)
      case _ => null
    }
  }

  private def variables(amount: Int = 1): List[Var] =
    (for (variableNumber <- 0 until amount) yield PrologVar(variableStart + variableNumber)).toList

  private[this] implicit class MyRichTerm(term: Term) {
    def toCard: Option[Card] = term toTupleTerm match {
      case Some(card) => (card getArg 0).toInt match {
        case Some(number) => Some(Card(number, card getArg 1 toString))
        case _ => None
      }
      case _ => None
    }

    def toTeam: Option[Team] = term toInt match {
      case Some(id) => Some(Team(id))
      case _ => None
    }
  }

  private implicit class PrologCard(card: Card) {
    def toTerm: Term = PrologTuple(card number,card seed)
  }

  private implicit def fromIntToString(value: Int): String = value toString
  private implicit def toOption[X](v: X): Option[X] = Option(v)
  private implicit def fromCardsToTerm(traversable: Traversable[Card]): Term = PrologStruct(traversable map(_.toTerm) toArray)

}

object PrologGameKnowledge {

  import alice.tuprolog.Prolog
  import TuPrologHelpers.prolog

  def apply(): GameKnowledgeFactory = (game: GameId) => new PrologGameKnowledge(game)

  private def createKnowledge(game: GameId): Prolog =
    prolog(COMMON_RULES_PATH,GAMES_PATH concat game.name.toLowerCase concat ".pl")

}

object TuPrologHelpers {

  import alice.tuprolog.SolveInfo

  implicit def fromStringToTerm(value: String): Term = Term createTerm value
  implicit def fromIntToTerm(value: Int): Term = Term createTerm value.toString
  implicit def fromVarToString(variable: Var): String = variable getName
  implicit def fromTraversableToPrologList(traversable: Traversable[Term]): Term = PrologStruct(traversable toArray)

  def prolog(files: String*): Prolog = {
    val engine = new Prolog()
    files foreach {f => engine addTheory PrologTheory(f)}
    engine
  }

  implicit class RichMap[K,V](map: Map[K,V]) {
    def valueOf[X](key: K)(toOption: V => Option[X]): Option[X] = map get key match {
      case Some(value) => toOption(value)
      case _ => None
    }
  }

  implicit class RichProlog(knowledge: Prolog) {

    def find[X](goal: Term, variable: Var)(termToOptionX: Term => Option[X]): Option[X] = find(goal) match {
      case Some(solution) => solution.valueOf(variable)(termToOptionX)
      case _ => None
    }

    def find(goal: Term): Option[Map[String,Term]] = knowledge solve goal match {
      case info if info.isSuccess => Some(solvedVars(info))
      case _ => None
    }

    def findAll(goal: Term): Set[Map[String,Term]] = {
      @scala.annotation.tailrec
      def _findAll(info: SolveInfo)(solutions: Set[Map[String,Term]]): Set[Map[String,Term]] = {
        if (info isSuccess) {
          val newSolutions = solutions + solvedVars(info)
          if (knowledge hasOpenAlternatives) _findAll(knowledge solveNext())(newSolutions) else newSolutions
        } else solutions
      }
      _findAll(knowledge solve goal)(Set())
    }

    def ?(goal: Term): Boolean = knowledge solve goal isSuccess

    def -(goal: Term): Option[Map[String,Term]] = find(RetractTerm(goal))

    def --(goal: Term): Set[Map[String,Term]] = findAll(RetractTerm(goal))

    def +(clauses: Struct*): Unit = knowledge addTheory PrologTheory(clauses)

    private[this] def solvedVars(info: SolveInfo): Map[String,Term] = {
      import scala.collection.JavaConverters._
      val solvedVars = for (v <- info.getBindingVars.asScala) yield (v getName, v getTerm)
      solvedVars filter (_ != null) toMap
    }

    private[this] object RetractTerm {
      private val retract = "retract"
      def apply(termToRetract: Term): Term = PrologStruct(retract, termToRetract)
    }
  }

  implicit class RichTerm(term: Term) {
    import alice.tuprolog.Number
    import scala.collection.JavaConverters._

    def toInt: Option[Int] = if (term.isInstanceOf[Number]) Some(term.toString.toInt) else term.toString.toOptionInt

    def toList: Option[List[Term]] =
      if (term.isList) {
        val list = term.asInstanceOf[Struct]
        Some(list.listIterator().asScala.toList)
      } else None

    def toTupleTerm: Option[PrologTuple] =
      if (term.isCompound)
        Some(PrologTuple(term.asInstanceOf[Struct]))
      else None
  }

  private implicit class RichString(value: String) {
    def toOptionInt: Option[Int] =
      try {
        Some(value.toInt)
      } catch {
        case _: Exception => None
      }
  }

  object PrologStruct {
    def apply(name: String, parameters: Term*): Struct = new Struct(name, parameters toArray)
    def apply(terms: Term*): Struct = PrologStruct(terms toArray)
    def apply(terms: Array[Term]): Struct = new Struct(terms)
    def apply(): Struct = new Struct()
  }

  object PrologTheory {
    def apply(clauses: Term*): Theory = new Theory(PrologStruct(clauses))
    def apply(path: String): Theory = new Theory(new FileInputStream(path))
  }

  trait PrologTuple {
    def getArg(index: Int): Term
  }

  object PrologTuple {
    private[this] val elementSeparator = ","
    def apply(values: String*): Term = Term createTerm values.mkString(elementSeparator)

    def apply(struct: Struct): PrologTuple = new PrologTupleImpl(struct)

    private[this] class PrologTupleImpl(struct: Struct) extends PrologTuple {
      override def getArg(index: Int): Term = struct getArg index
    }
  }

  object PrologVar {
    def apply(name: String): Var = new Var(name)
  }
}