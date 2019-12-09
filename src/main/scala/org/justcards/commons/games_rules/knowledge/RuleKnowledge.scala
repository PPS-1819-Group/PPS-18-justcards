package org.justcards.commons.games_rules.knowledge

import org.justcards.commons.Card
import org.justcards.commons.games_rules.PointsConversion.PointsConversion
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting

trait RuleKnowledge {
  def cardsDistribution(cardsInHand: Int, cardsToDrawPerTurn: Int, cardsOnField: Int): Boolean
  def playSameSeed(value: Boolean): Boolean
  def chooseBriscola(setting: BriscolaSetting): Boolean
  def pointsToWinSession(points: Int): Boolean
  def pointsObtainedInAMatch(conversion: PointsConversion): Boolean
  def winnerPoints(conversion: PointsConversion): Boolean
  def loserPoints(conversion: PointsConversion): Boolean
  def drawPoints(conversion: PointsConversion): Boolean
  def starterCard(card: Card): Boolean
  def lastTakeWorthOneMorePoint(value: Boolean): Boolean
  def cardsHierarchyAndPoints(values: List[(Int,Int)]): Boolean
  def deckCards: (Set[Card], Set[String])
}

object RuleKnowledge {
  def apply(): RuleKnowledge = PrologRuleKnowledge()
  val RULES_PATH: String = "src/main/resources/org/justcards/rules/"
}
