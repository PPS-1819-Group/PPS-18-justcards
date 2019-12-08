package org.justcards.commons.games_rules.knowledge

import alice.tuprolog.Prolog
import org.justcards.commons.Card
import org.justcards.commons.games_rules.PointsConversion._
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting

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

  override def cardsHierarchyAndPoints(values: List[(Int, Int)]): Boolean = values forall {v =>
    knowledge ? PrologStruct(card, v._1, PrologVar()) && knowledge ? PrologStruct(points,v._2)
  }

  private def ?(conversion: PointsConversion): Boolean = conversion match {
    case MATCH_POINTS => true
    case _ => knowledge ? PrologStruct(points,conversion.value)
  }
}

object PrologRuleKnowledge {
  import RuleKnowledge.RULES_PATH
  import org.justcards.commons.helper.TuPrologHelpers.prolog

  private[this] val editableRules = "editableRules.pl"

  def apply(): PrologRuleKnowledge = new PrologRuleKnowledge(prolog(RULES_PATH + editableRules))
}
