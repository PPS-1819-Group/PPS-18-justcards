package org.justcards.commons.games_rules.converter
import alice.tuprolog.{Struct, Term}
import org.justcards.commons.games_rules.{PointsConversion, Rule}
import org.justcards.commons.games_rules.PointsConversion._
import org.justcards.commons.Card
import org.justcards.server.Commons.BriscolaSetting
import org.justcards.server.Commons.BriscolaSetting._

class PrologGameRulesConverter extends GameRulesConverter {

  import org.justcards.commons.games_rules.Rule._
  import org.justcards.commons.helper.TuPrologHelpers._
  import org.justcards.commons.helper.TuPrologHelpers.PrologOperation.PrologOperator._
  import org.justcards.commons.helper.PrologExtensions._
  import PrologGameRulesConverter._

  private def intToString(x: Int): String = PrologInt(x)

  private def boolToString(x: Boolean): String = PrologBoolean(x)

  private def tripleIntToString(x: (Int, Int, Int)): String = PrologTuple(x._1,x._2,x._3)

  private def briscolaSettingToString(x: BriscolaSetting): String = PrologStruct(x toString)

  private def cardToString(x: Card): String = x toTerm

  private def cardsHierarchyAndPointsToString(x: CardsHierarchyAndPoints): String = fromTraversableToPrologList(x.map(v => PrologTuple(v._1,v._2)))

  private def pointsConversionToString(x: PointsConversion): String = PrologTuple(x toString,x value)

  private def parseToInt(x: String): Option[Int] = Term createTerm x toInt

  private def parseToBoolean(x: String): Option[Boolean] = Term createTerm x toBoolean

  private def parseToCardsDistribution(x: String): Option[CardsDistribution] = (Term createTerm x) toList match {
    case Some(info) if info.size >= 3 => (info.head.toInt, info(1).toInt, info(2).toInt) match {
      case (Some(h), Some(d), Some(f)) => Some((h,d,f))
      case _ => None
    }
    case _ => None
  }

  private def parseToBriscolaSetting(x: String): Option[BriscolaSetting] = Term createTerm x toBriscolaSetting

  private def parseToPointsConversion(x: String): Option[PointsConversion] = (Term createTerm x) toList match {
      case Some(info) => info.head toPointsConversion match {
        case Some(MATCH_POINTS) => Some(MATCH_POINTS)
        case Some(EXACTLY) => info(1) toInt match {
          case Some(v) => Some(EXACTLY.value(v))
          case _ => None
        }
        case Some(DIVIDE) => info(1) toInt match {
          case Some(v) => Some(DIVIDE.value(v))
          case _ => None
        }
        case _ => None
      }
      case _ => None
    }

  private def parseToCard(x: String): Option[Card] = Term createTerm x toCard

  private def parseToCardsHierarcyAndPoints(x: String): Option[CardsHierarchyAndPoints] = (Term createTerm x) toList match {
    case Some(values) => Some(values
      .map(_.toList)
      .collect{case Some(v) if v.size >= 2 => (v.head toInt, v(1) toInt)}
      .collect{case (Some(h),Some(p)) => (h,p)}
    )
    case _ => None
  }

  private implicit def termToString(x: Term): String = x toString
  private implicit def structToString(x: Struct): String = x toString

  override def serialize(rules: Map[String, Any]): Map[String, String] = {
    rules.map{r =>
      Rule.find(r._1) match {
        case Some(CARDS_DISTRIBUTION) => r._2 match {
          case (h:Int, d:Int, f:Int) => Some((r._1,tripleIntToString((h,d,f))))
          case _ => None
        }
        case Some(PLAY_SAME_SEED) if r._2.isInstanceOf[Boolean] => Some((r._1,boolToString(r._2.asInstanceOf[Boolean])))
        case Some(CHOOSE_BRISCOLA) if r._2.isInstanceOf[BriscolaSetting] => Some((r._1,briscolaSettingToString(r._2.asInstanceOf[BriscolaSetting])))
        case Some(POINTS_TO_WIN_SESSION) if r._2.isInstanceOf[Int] => Some((r._1,intToString(r._2.asInstanceOf[Int])))
        case Some(POINTS_OBTAINED_IN_A_MATCH) if r._2.isInstanceOf[PointsConversion] => Some((r._1,pointsConversionToString(r._2.asInstanceOf[PointsConversion])))
        case Some(WINNER_POINTS) if r._2.isInstanceOf[PointsConversion] => Some((r._1,pointsConversionToString(r._2.asInstanceOf[PointsConversion])))
        case Some(LOSER_POINTS) if r._2.isInstanceOf[PointsConversion] => Some((r._1,pointsConversionToString(r._2.asInstanceOf[PointsConversion])))
        case Some(DRAW_POINTS) if r._2.isInstanceOf[PointsConversion] => Some((r._1,pointsConversionToString(r._2.asInstanceOf[PointsConversion])))
        case Some(STARTER_CARD) if r._2.isInstanceOf[Card] => Some((r._1,cardToString(r._2.asInstanceOf[Card])))
        case Some(LAST_TAKE_WORTH_ONE_MORE_POINT) if r._2.isInstanceOf[Boolean] => Some((r._1,boolToString(r._2.asInstanceOf[Boolean])))
        case Some(CARDS_HIERARCHY_AND_POINTS) => r._2 match {
          case h::t => Some((r._1,cardsHierarchyAndPointsToString(h::t collect { case (number: Int, points: Int) => (number,points) })))
          case _ => None
        }
      }
    } collect {case Some(v) => v} toMap;
  }

  private def getRule[X](rules: Map[String,String], rule: Rule[X])(convert: String => Option[X]): Option[(String,X)] = {
    rules get rule.toString match {
      case Some(value) =>
        val concreteValue: Option[X] = convert(value)
        if (concreteValue.isDefined) Some((rule.toString,concreteValue get)) else None
      case _ => None
    }
  }

  override def deserialize(rules: Map[String, String]): Map[String, Any] = {
    Set(
      getRule(rules,CARDS_DISTRIBUTION)(parseToCardsDistribution),
      getRule(rules,PLAY_SAME_SEED)(parseToBoolean),
      getRule(rules,CHOOSE_BRISCOLA)(parseToBriscolaSetting),
      getRule(rules,POINTS_TO_WIN_SESSION)(parseToInt),
      getRule(rules,POINTS_OBTAINED_IN_A_MATCH)(parseToPointsConversion),
      getRule(rules,WINNER_POINTS)(parseToPointsConversion),
      getRule(rules,LOSER_POINTS)(parseToPointsConversion),
      getRule(rules,DRAW_POINTS)(parseToPointsConversion),
      getRule(rules,STARTER_CARD)(parseToCard),
      getRule(rules,LAST_TAKE_WORTH_ONE_MORE_POINT)(parseToBoolean),
      getRule(rules,CARDS_HIERARCHY_AND_POINTS)(parseToCardsHierarcyAndPoints)
    ).collect{ case Some(v) => v }.toMap
  }
}

object PrologGameRulesConverter {

  def apply(): PrologGameRulesConverter = new PrologGameRulesConverter()

  implicit class PrologGameRulesConverterRichTerm(term: Term) {

    import org.justcards.commons.helper.TuPrologHelpers._

    def toBriscolaSetting: Option[BriscolaSetting] =
      try {
        Some(BriscolaSetting.withName(term.stringValue))
      } catch {
        case _: Exception => None
      }
    def toPointsConversion: Option[PointsConversion] =
      try {
        Some(PointsConversion.withName(term.stringValue))
      } catch {
        case _: Exception => None
      }
  }
}
