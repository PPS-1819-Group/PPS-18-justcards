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
  import org.justcards.commons.helper.PrologExtensions._
  import PrologGameRulesConverter._

  private trait Serializable[X]{
    def serialize(x: X): String
    def deserialize(x: String): Option[X]
  }

  private implicit object IntSerializable extends Serializable[Int] {
    override def serialize(x: Int): String = PrologInt(x)

    override def deserialize(x: String): Option[Int] = Term createTerm x toInt
  }

  private implicit object BoolSerializable extends Serializable[Boolean] {
    override def serialize(x: Boolean): String = PrologBoolean(x)

    override def deserialize(x: String): Option[Boolean] = Term createTerm x toBoolean
  }

  private implicit object CardsDistributionSerializable extends Serializable[CardsDistribution] {
    override def serialize(x: CardsDistribution): String = PrologTuple(x._1,x._2,x._3)

    override def deserialize(x: String): Option[(Int, Int, Int)] = (Term createTerm x) toList match {
      case Some(info) if info.size >= 3 => (info.head.toInt, info(1).toInt, info(2).toInt) match {
        case (Some(h), Some(d), Some(f)) => Some((h,d,f))
        case _ => None
      }
      case _ => None
    }
  }

  private implicit object BriscolaSettingSerializable extends Serializable[BriscolaSetting] {
    override def serialize(x: BriscolaSetting): String = PrologStruct(x toString)

    override def deserialize(x: String): Option[BriscolaSetting] = Term createTerm x toBriscolaSetting
  }

  private implicit object CardSerializable extends Serializable[Card] {
    override def serialize(x: Card): String = x toTerm

    override def deserialize(x: String): Option[Card] = Term createTerm x toCard
  }

  private implicit object CardsHierarchyAndPointsSerializable extends Serializable[CardsHierarchyAndPoints] {
    override def serialize(x: CardsHierarchyAndPoints): String = fromTraversableToPrologList(x.map(v => PrologTuple(v._1,v._2)))

    override def deserialize(x: String): Option[CardsHierarchyAndPoints] = (Term createTerm x) toList match {
      case Some(values) => Some(values
        .map(_.toList)
        .collect{case Some(v) if v.size >= 2 => (v.head toInt, v(1) toInt)}
        .collect{case (Some(h),Some(p)) => (h,p)}
      )
      case _ => None
    }
  }


  private implicit object PointsConversionSerializable extends Serializable[PointsConversion] {
    override def serialize(x: PointsConversion): String = PrologTuple(x toString,x value)

    override def deserialize(x: String): Option[PointsConversion] = (Term createTerm x) toList match {
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
  }

  private def serializeToString[X: Serializable](x: X): String = implicitly[Serializable[X]].serialize(x)

  private implicit def termToString(x: Term): String = x toString
  private implicit def structToString(x: Struct): String = x toString

  override def serialize(rules: Map[Rule.Value, Any]): Map[String, String] = rules collect {
    case (CARDS_DISTRIBUTION,(h:Int,d:Int,f:Int)) => (CARDS_DISTRIBUTION.toString,serializeToString((h,d,f)))
    case (PLAY_SAME_SEED,v: Boolean) => (PLAY_SAME_SEED.toString,serializeToString(v))
    case (CHOOSE_BRISCOLA,v: BriscolaSetting) => (CHOOSE_BRISCOLA.toString,serializeToString(v))
    case (POINTS_TO_WIN_SESSION,v :Int) => (POINTS_TO_WIN_SESSION.toString,serializeToString(v))
    case (POINTS_OBTAINED_IN_A_MATCH,v: PointsConversion) => (POINTS_OBTAINED_IN_A_MATCH.toString,serializeToString(v))
    case (WINNER_POINTS,v: PointsConversion) => (WINNER_POINTS.toString,serializeToString(v))
    case (LOSER_POINTS,v: PointsConversion) => (LOSER_POINTS.toString,serializeToString(v))
    case (DRAW_POINTS,v: PointsConversion) => (DRAW_POINTS.toString,serializeToString(v))
    case (STARTER_CARD,v: Card) => (STARTER_CARD.toString, serializeToString(v))
    case (LAST_TAKE_WORTH_ONE_MORE_POINT,v: Boolean) => (LAST_TAKE_WORTH_ONE_MORE_POINT.toString, serializeToString(v))
    case (CARDS_HIERARCHY_AND_POINTS,v: List[Any]) =>
      (CARDS_HIERARCHY_AND_POINTS.toString, serializeToString(v collect { case (number: Int, points: Int) => (number, points) }))
  }

  private def getRule[X: Serializable](rules: Map[String,String], rule: Rule[X]): Option[(Rule.Value,X)] = {
    rules get rule.toString match {
      case Some(value) =>
        val concreteValue: Option[X] = implicitly[Serializable[X]].deserialize(value)
        if (concreteValue.isDefined) Some((rule,concreteValue get)) else None
      case _ => None
    }
  }

  override def deserialize(rules: Map[String, String]): Map[Rule.Value, Any] = {
    Set(
      getRule(rules,CARDS_DISTRIBUTION),
      getRule(rules,PLAY_SAME_SEED),
      getRule(rules,CHOOSE_BRISCOLA),
      getRule(rules,POINTS_TO_WIN_SESSION),
      getRule(rules,POINTS_OBTAINED_IN_A_MATCH),
      getRule(rules,WINNER_POINTS),
      getRule(rules,LOSER_POINTS),
      getRule(rules,DRAW_POINTS),
      getRule(rules,STARTER_CARD),
      getRule(rules,LAST_TAKE_WORTH_ONE_MORE_POINT),
      getRule(rules,CARDS_HIERARCHY_AND_POINTS)
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
