package org.justcards.commons.games_rules.knowledge

import alice.tuprolog.Prolog
import org.justcards.commons.Card
import org.justcards.commons.games_rules.PointsConversion
import org.justcards.commons.games_rules.PointsConversionType._
import org.justcards.commons.games_rules.BriscolaSetting.BriscolaSetting

class PrologRuleKnowledge(private val knowledge: Prolog) extends RuleKnowledge {

  import org.justcards.commons.helper.TuPrologHelpers._

  private val cardsDistribution = "cardsDistribution"
  private val pointsToWinSession = "pointsToWinSession"
  private val points = "points"
  private val starterCard = "starterCard"
  private val card = "card"

  override def cardsDistribution(cardsInHand: Int, cardsToDrawPerTurn: Int, cardsOnField: Int): Boolean =
    knowledge ? PrologStruct(cardsDistribution,cardsInHand,cardsToDrawPerTurn,cardsOnField)

  override def playSameSeed(value: Boolean): Boolean = value.isInstanceOf[Boolean]

  override def chooseBriscola(setting: BriscolaSetting): Boolean = setting.isInstanceOf[BriscolaSetting]

  override def pointsToWinSession(points: Int): Boolean = knowledge ? PrologStruct(pointsToWinSession,points)

  override def pointsObtainedInAMatch(conversion: PointsConversion): Boolean = true

  override def winnerPoints(conversion: PointsConversion): Boolean = ?(conversion)

  override def loserPoints(conversion: PointsConversion): Boolean = ?(conversion)

  override def drawPoints(conversion: PointsConversion): Boolean = ?(conversion)

  override def starterCard(card: Card): Boolean = knowledge ? PrologStruct(starterCard,card number, card seed)

  override def lastTakeWorthOneMorePoint(value: Boolean): Boolean = value.isInstanceOf[Boolean]

  override def cardsHierarchyAndPoints(values: List[(Int, Int)]): Boolean = {
    val vars = PrologVar(amount = 2)
    val number = vars head
    val cardsNumber = knowledge findAll PrologStruct(card, number, vars(1)) map (_.valueOf(number)(_.toInt)) collect {
      case Some(v) => v
    }
    val areAllValuesAllowed = values forall { v => (cardsNumber contains v._1) && knowledge ? PrologStruct(points,v._2) }
    areAllValuesAllowed && cardsNumber.size == values.size
  }

  override def deckCards: (Set[Card], Set[String]) = {
    val vars = PrologVar(amount = 2)
    val number = vars head
    val seed = vars(1)
    val cardsFound = (for (solution <- knowledge findAll PrologStruct(card,number,seed))
      yield (solution.valueOf(number)(_.toInt),solution.valueOf(seed)(_.toString)))
      .collect { case (Some(cardNumber), Some(cardSeed)) => Card(cardNumber,cardSeed) }
    (cardsFound, cardsFound map(_.seed))
  }

  private def ?(conversion: PointsConversion): Boolean = conversion match {
    case MATCH_POINTS => true
    case _ => knowledge ? PrologStruct(points,conversion.value)
  }

  private implicit def toOption[X](v: X): Option[X] = Option(v)
}

object PrologRuleKnowledge {
  import RuleKnowledge.RULES_PATH
  import org.justcards.commons.helper.TuPrologHelpers.prolog

  private[this] val editableRules = "editableRules.pl"

  def apply(): PrologRuleKnowledge = new PrologRuleKnowledge(prolog(getClass.getResourceAsStream(RULES_PATH + editableRules)))
}
