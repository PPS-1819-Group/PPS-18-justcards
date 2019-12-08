package org.justcards.commons.games_rules

import org.justcards.commons.Card
import org.justcards.commons.games_rules.knowledge.RuleKnowledge
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting

object PointsConversion extends Enumeration {
  type PointsConversion = PointsConversionVal
  val EXACTLY = PointsConversionVal()
  val DIVIDE = PointsConversionVal()
  val MATCH_POINTS = PointsConversionVal()

  sealed case class PointsConversionVal(var value: Int = 0) extends Val(nextId, nextId toString) {
    def value(v: Int): PointsConversion = {
      value = v
      this
    }
  }

  implicit def valueToRule(x: Value): PointsConversion = x.asInstanceOf[PointsConversion]

}

object Rule extends Enumeration {

  import PointsConversion._

  private val rulesKnowledge = RuleKnowledge()

  type Rule[X] = RuleVal[X]
  type CardsDistribution = (Int,Int,Int)
  type CardsHierarchyAndPoints = List[(Int,Int)]

  val CARDS_DISTRIBUTION = RuleVal[CardsDistribution](v => rulesKnowledge.cardsDistribution(v._1,v._2,v._3))
  val PLAY_SAME_SEED = RuleVal[Boolean](rulesKnowledge playSameSeed)
  val CHOOSE_BRISCOLA = RuleVal[BriscolaSetting](rulesKnowledge chooseBriscola)
  val POINTS_TO_WIN_SESSION = RuleVal[Int](rulesKnowledge pointsToWinSession)
  val POINTS_OBTAINED_IN_A_MATCH = RuleVal[PointsConversion](rulesKnowledge pointsObtainedInAMatch)
  val WINNER_POINTS = RuleVal[PointsConversion](rulesKnowledge winnerPoints)
  val LOSER_POINTS = RuleVal[PointsConversion](rulesKnowledge loserPoints)
  val DRAW_POINTS = RuleVal[PointsConversion](rulesKnowledge drawPoints)
  val STARTER_CARD = RuleVal[Card](rulesKnowledge starterCard)
  val LAST_TAKE_WORTH_ONE_MORE_POINT = RuleVal[Boolean](rulesKnowledge lastTakeWorthOneMorePoint)
  val CARDS_HIERARCHY_AND_POINTS = RuleVal[CardsHierarchyAndPoints](rulesKnowledge cardsHierarchyAndPoints)

  sealed case class RuleVal[-X](private val allow: X => Boolean) extends Val(nextId, nextId toString) {
    def isAllowed(x: X): Boolean = allow(x)
  }

  def find[X](x: String): Option[Rule[X]] = Rule.values.find(_.toString == x)

  implicit def valueToRule[X](x: Value): Rule[X] = x.asInstanceOf[RuleVal[X]]

  private[this] implicit def optionValueToOptionRule[X](x: Option[Value]): Option[Rule[X]] = x match {
    case Some(rule) => Some(rule)
    case _ => None
  }
}