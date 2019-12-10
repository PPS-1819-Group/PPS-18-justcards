package org.justcards.server.knowledge_engine.game_knowledge

import alice.tuprolog.{Prolog, Term, Var}
import org.justcards.commons.{Card, GameId}
import org.justcards.server.Commons.{Team, UserInfo}
import org.justcards.server.Commons.BriscolaSetting._
import org.justcards.server.Commons.Team.Team
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge._

class PrologGameKnowledge(private val game: GameId) extends GameKnowledge {

  import PrologGameKnowledge._
  import org.justcards.commons.helper.TuPrologHelpers._
  import org.justcards.commons.helper.PrologExtensions._

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
  private val sessionStarter = "sessionStarterPlayer"
  private val variableStart = "VAR"
  private val knowledge: Prolog = createKnowledge(game)

  override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = {
    val variable = PrologVar()
    val cardsInHand = knowledge.find(PrologStruct(hand,variable),variable)(_.toInt)
    val cardsToDraw = knowledge.find(PrologStruct(draw,variable),variable)(_.toInt)
    (cardsInHand getOrElse defaultCardsInHand, cardsToDraw getOrElse defaultCardsToDraw, defaultCardsOnField)
  }

  override def deckCards: Set[Card] = {
    val vars = PrologVar(amount = 2)
    val number = vars head
    val seed = vars(1)
    val cardsFound = for (solution <- knowledge findAll PrologStruct(card,number,seed))
      yield (solution.valueOf(number)(_.toInt),solution.valueOf(seed)(_.toString))
    cardsFound collect { case (Some(cardNumber), Some(cardSeed)) => Card(cardNumber,cardSeed) }
  }

  override def seeds: Set[Seed] = {
    val seedVar = PrologVar()
    (for (solution <- knowledge findAll PrologStruct(seed,seedVar)) yield solution.valueOf(seedVar)(_.toString))
      .collect{case Some(value) => value}
  }

  override def hasToChooseBriscola: BriscolaSetting = {
    val setting = PrologVar()
    knowledge.find(PrologStruct(briscolaSetting,setting),setting)(_.toInt) match {
      case Some(1) => USER
      case Some(0) => SYSTEM
      case _ => NOT_BRISCOLA
    }
  }

  override def setBriscola(seed: Seed): Boolean = {
    val briscolaValid = knowledge ? PrologStruct(this.seed, seed)
    if (briscolaValid) {
      knowledge -- PrologStruct(currentBriscola,PrologVar())
      knowledge + PrologStruct(currentBriscola,seed)
    }
    briscolaValid
  }

  override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = {
    val newFieldCards = PrologVar()
    knowledge.find(PrologStruct(turn, card toTerm, fieldCards, handCards, newFieldCards), newFieldCards)(_.toList) match {
      case Some(list) => Some(list map(_.toCard) filter(_.isDefined) map(_.get))
      case _ => None
    }
  }

  override def handWinner(fieldCards: List[(Card, UserInfo)]): UserInfo = {
    val winner = PrologVar()
    knowledge.find(
      PrologStruct(fieldWinner, fieldCards map(v => PrologTuple(v._1.number,v._1.seed,v._2.username)), winner),
      winner
    )(_.toString) match {
      case Some(player) => fieldCards map(_._2) find(_.username == player) orNull
      case _ => null
    }
  }

  override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = {
    val vars = PrologVar(amount = 3)
    val teamWinner = vars head
    val firstTeamPoints = vars(1)
    val secondTeamPoints = vars(2)
    knowledge.find(
      PrologStruct(matchWinner,
        firstTeamCards, secondTeamCards, lastHandWinner id,
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
    val winner = PrologVar()
    knowledge.find(PrologStruct(sessionWinner,firstTeamPoints,secondTeamPoints, winner),winner)(_.toTeam)
  }

  override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = {
    val vars = PrologVar(amount = 2)
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

  override def sessionStarterPlayer(playersHandCards: Set[(UserInfo, Set[Card])]): Option[UserInfo] = {
    val starter = PrologVar()
    knowledge.find(
      PrologStruct(sessionStarter, playersHandCards map(v => PrologTuple(v._1.username,v._2)), starter),
      starter
    )(_.toString) match {
      case Some(player) => playersHandCards map(_._1) find(_.username == player)
      case _ => None
    }
  }

  private[this] implicit class MyRichTerm(term: Term) {
    def toTeam: Option[Team] = term toInt match {
      case Some(id) => Some(Team(id))
      case _ => None
    }
  }

  private implicit def fromIntToString(value: Int): String = value toString
  private implicit def toOption[X](v: X): Option[X] = Option(v)
  private implicit def fromCardsToTerm(traversable: Traversable[Card]): Term = PrologStruct(traversable map(_.toTerm) toArray)
}

object PrologGameKnowledge {

  import alice.tuprolog.Prolog
  import org.justcards.commons.helper.TuPrologHelpers.prolog

  def apply(): GameKnowledgeFactory = (game: GameId) => new PrologGameKnowledge(game)

  private def createKnowledge(game: GameId): Prolog =
    prolog(COMMON_RULES_PATH,GAMES_PATH concat game.name.toLowerCase concat ".pl")

}