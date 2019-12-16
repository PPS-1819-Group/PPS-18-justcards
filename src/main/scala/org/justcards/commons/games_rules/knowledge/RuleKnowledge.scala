package org.justcards.commons.games_rules.knowledge

import org.justcards.commons.Card
import org.justcards.commons.games_rules.BriscolaSetting.BriscolaSetting
import org.justcards.commons.games_rules.PointsConversion

/**
 * A RuleKnowledge to determine if the rules values are allowed.
 */
trait RuleKnowledge {
  /**
   * Determine if the cards distribution is allowed.
   * @param cardsInHand number cards in the hand of a player on the match startup
   * @param cardsToDrawPerTurn number cards to be drawn every turn by each player
   * @param cardsOnField number cards on the field on the match startup
   * @return true if allowed else false
   */
  def cardsDistribution(cardsInHand: Int, cardsToDrawPerTurn: Int, cardsOnField: Int): Boolean

  /**
   * Determine if the "PlaySameSeed" value is allowed.
   * @param value the value
   * @return if allowed
   */
  def playSameSeed(value: Boolean): Boolean

  /**
   * Determine if the the way to set Briscola is allowed.
   * @param setting the way to set Briscola
   * @return if allowed
   */
  def chooseBriscola(setting: BriscolaSetting): Boolean

  /**
   * Determine if the specified number of points to win a session is allowed.
   * @param points the number of points to win a session
   * @return if allowed
   */
  def pointsToWinSession(points: Int): Boolean

  /**
   * Determine if the points conversion for the points obtained in a match is allowed.
   * @param conversion the conversion
   * @return if allowed
   */
  def pointsObtainedInAMatch(conversion: PointsConversion): Boolean

  /**
   * Determine if the points conversion for the points to give to the winner is allowed.
   * @param conversion the conversion
   * @return if allowed
   */
  def winnerPoints(conversion: PointsConversion): Boolean

  /**
   * Determine if the points conversion for the points to give to the loser is allowed.
   * @param conversion the conversion
   * @return if allowed
   */
  def loserPoints(conversion: PointsConversion): Boolean

  /**
   * Determine if the points conversion for the points to give in the case of a draw is allowed.
   * @param conversion the conversion
   * @return if allowed
   */
  def drawPoints(conversion: PointsConversion): Boolean

  /**
   * Determine if the specified card can be used as starter card.
   * @param card the card
   * @return if allowed
   */
  def starterCard(card: Card): Boolean

  /**
   * Determine if the "LastTakeWorthOneMorePoint" value is allowed.
   * @param value the value
   * @return if allowed
   */
  def lastTakeWorthOneMorePoint(value: Boolean): Boolean

  /**
   * Determine if the specified cards points and hierarchy are allowed.
   * @param values the cards
   * @return if allowed
   */
  def cardsHierarchyAndPoints(values: List[(Int,Int)]): Boolean

  /**
   * Retrieve the available cards.
   * @return the available cards
   */
  def deckCards: (Set[Card], Set[String])
}

object RuleKnowledge {
  def apply(): RuleKnowledge = PrologRuleKnowledge()
  val RULES_PATH: String = "/org/justcards/rules/"
}
