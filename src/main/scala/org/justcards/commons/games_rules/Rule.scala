package org.justcards.commons.games_rules

import org.justcards.commons.Card
import org.justcards.commons.games_rules.PointsConversionType.PointsConversionType
import org.justcards.commons.games_rules.knowledge.RuleKnowledge

/**
 * Available points conversion types.
 */
object PointsConversionType extends Enumeration {
  type PointsConversionType = Value
  val EXACTLY, DIVIDE, MATCH_POINTS = Value
}

/**
 * A Points conversion.
 * @param typology the points conversion type
 * @param value the value to be used if needed
 */
case class PointsConversion(typology: PointsConversionType, value: Int = 0)

/**
 * Available Briscola settings to determine how the briscola will be chosen.
 */
object BriscolaSetting extends Enumeration {
  type BriscolaSetting = Value
  val USER, SYSTEM, NOT_BRISCOLA = Value
}

/**
 * Available custom rules for the creation of a game.
 */
object Rule extends Enumeration {

  import org.justcards.commons.games_rules.BriscolaSetting.BriscolaSetting

  private val rulesKnowledge = RuleKnowledge()

  type Rule[X] = RuleVal[X]
  type CardsDistribution = (Int,Int,Int)
  type CardsHierarchyAndPoints = List[(Int,Int)]

  val CARDS_DISTRIBUTION: Rule[CardsDistribution] = RuleVal(v => rulesKnowledge.cardsDistribution(v._1,v._2,v._3))
  val PLAY_SAME_SEED: Rule[Boolean] = RuleVal(rulesKnowledge playSameSeed)
  val CHOOSE_BRISCOLA: Rule[BriscolaSetting] = RuleVal(rulesKnowledge chooseBriscola)
  val POINTS_TO_WIN_SESSION: Rule[Int] = RuleVal(rulesKnowledge pointsToWinSession)
  val POINTS_OBTAINED_IN_A_MATCH: Rule[PointsConversion] = RuleVal(rulesKnowledge pointsObtainedInAMatch)
  val WINNER_POINTS: Rule[PointsConversion] = RuleVal(rulesKnowledge winnerPoints)
  val LOSER_POINTS: Rule[PointsConversion] = RuleVal(rulesKnowledge loserPoints)
  val DRAW_POINTS: Rule[PointsConversion] = RuleVal(rulesKnowledge drawPoints)
  val STARTER_CARD: Rule[Card] = RuleVal(rulesKnowledge starterCard)
  val LAST_TAKE_WORTH_ONE_MORE_POINT: Rule[Boolean] = RuleVal(rulesKnowledge lastTakeWorthOneMorePoint)
  val CARDS_HIERARCHY_AND_POINTS: Rule[CardsHierarchyAndPoints] = RuleVal(rulesKnowledge cardsHierarchyAndPoints)

  sealed case class RuleVal[-X](private val allow: X => Boolean) extends Val(nextId, nextId toString) {
    def isAllowed(x: X): Boolean = allow(x)
  }

  /**
   * Retrieve a rule if exists.
   * @param x the rule name
   * @tparam X the rule value type
   * @return the rule
   */
  def find[X](x: String): Option[Rule[X]] = Rule.values.find(_.toString == x)

  /**
   * The available cards for a new game.
   * @return the cards
   */
  def deckCards: (Set[Card], Set[String]) = rulesKnowledge deckCards

  /**
   * The mandatory rules to be specified.
   * @return the mandatory rules
   */
  def mandatoryRules: Set[Rule.Value] = Rule.values - STARTER_CARD

  implicit def valueToRule[X](x: Value): Rule[X] = x.asInstanceOf[RuleVal[X]]

  private[this] implicit def optionValueToOptionRule[X](x: Option[Value]): Option[Rule[X]] = x match {
    case Some(rule) => Some(rule)
    case _ => None
  }
}