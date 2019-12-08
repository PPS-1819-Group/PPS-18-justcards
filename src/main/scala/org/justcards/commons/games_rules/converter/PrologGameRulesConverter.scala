package org.justcards.commons.games_rules.converter
import alice.tuprolog.{Prolog, Struct, Term}
import org.justcards.commons.games_rules.{GameRules, PointsConversion, Rule}
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

  private val startHand = "startHand"
  private val draw = "draw"
  private val playSameSeed = "playSameSeed"
  private val chooseBriscola = "chooseBriscola"
  private val pointsToWinSession = "pointsToWinSession"
  private val winnerPoints = "winnerPoints"
  private val loserPoints = "loserPoints"
  private val drawPoints = "drawPoints"
  private val points = "points"
  private val starterCard = "starterCard"
  private val lastTakeHasOneMorePoint = "lastTakeHasOneMorePoint"
  private val card = "card"
  private val seed = "seed"
  private val newLinePattern = "\\n"

  override def parse(rules: GameRules): List[String] = {
    val theories: List[Term] = (rules flatMap { r =>
      Rule.find(r._1) match {
        case Some(CARDS_DISTRIBUTION) => (Term createTerm r._2) toList match {
          case Some(info) if info.size >= 3 => (info.head.toInt, info(1).toInt, info(2).toInt) match {
            case (Some(h), Some(d), Some(_)) => List(
              PrologStruct(startHand, PrologInt(h)),
              PrologStruct(draw, PrologInt(d))
            )
            case _ => List()
          }
          case _ => List()
        }
        case Some(PLAY_SAME_SEED) => simpleRule(r._2,playSameSeed)(_.toBoolean)()(Some(v => v))
        case Some(POINTS_TO_WIN_SESSION) => simpleRule(r._2,pointsToWinSession)(_.toInt)(Some(v => Seq(PrologInt(v))))()
        case Some(CHOOSE_BRISCOLA) => (Term createTerm r._2) toBriscolaSetting match {
          case Some(USER) => List(PrologStruct(chooseBriscola, PrologInt(1)))
          case Some(SYSTEM) => List(PrologStruct(chooseBriscola, PrologInt(0)))
          case Some(NOT_BRISCOLA) => List(PrologStruct(chooseBriscola, PrologInt(-1)))
          case _ => List()
        }
        case Some(POINTS_OBTAINED_IN_A_MATCH) => createPointsRule(r._2,points)
        case Some(STARTER_CARD) => simpleRule(r._2,starterCard)(_.toCard)(Some(c => Seq(c number, c seed)))()
        case Some(LAST_TAKE_WORTH_ONE_MORE_POINT) => simpleRule(r._2,lastTakeHasOneMorePoint)(_.toBoolean)()(accept = Some(v => v))
        case Some(CARDS_HIERARCHY_AND_POINTS) => (Term createTerm r._2) toList match {
          case Some(list) =>
            val seedVar = PrologVar().head
            val dataList = list.map(_.toList).filter(_.isDefined).map(_.get)
            for (i <- list.indices; info = dataList(i))
              yield PrologClause(PrologStruct(name = card, info head,seedVar,i + 1,info(1)),PrologStruct(seed,seedVar))
          case _ => List()
        }
        case Some(WINNER_POINTS) => createPointsRule(r._2,winnerPoints)
        case Some(LOSER_POINTS) => createPointsRule(r._2,loserPoints)
        case Some(DRAW_POINTS) => createPointsRule(r._2,drawPoints)
        case _ => List()
      }
    }).toList
    val prolog = new Prolog()
    prolog.+(theories: _*)
    prolog.getTheory.toString.split(newLinePattern).filter(!_.isBlank).toList
  }

  override def apply(x: Int): String = PrologInt(x)

  override def apply(x: Boolean): String = PrologBoolean(x)

  override def apply(x: (Int, Int, Int)): String = PrologTuple(x._1,x._2,x._3)

  override def apply(x: BriscolaSetting): String = PrologStruct(x toString)

  override def apply(x: Card): String = x toTerm

  override def apply(x: List[(Int,Int)]): String = fromTraversableToPrologList(x.map(v => PrologTuple(v._1,v._2)))

  override def apply(x: PointsConversion): String = PrologTuple(x toString,x value)

  private def createPointsRule(value: String, name: String): List[Term] = (Term createTerm value) toList match {
    case Some(info) => info.head toPointsConversion match {
      case Some(MATCH_POINTS) =>
        val v = PrologVar().head
        List(PrologStruct(name, v, v))
      case Some(EXACTLY) =>
        val v = PrologVar().head
        List(PrologStruct(name, v, info(1)))
      case Some(DIVIDE) =>
        val vars = PrologVar(2)
        val x = vars head
        val y = vars(1)
        List(PrologClause(PrologStruct(name, x, y), PrologOperation(IS, y, PrologOperation(DIVISION, x, info(1)))))
      case _ => List()
    }
    case _ => List()
  }

  private def simpleRule[X](value: String, name: String)
                           (mapper: Term => Option[X])
                           (getArgs: Option[X => Seq[Term]] = None)
                           (accept: Option[X => Boolean] = None): List[Term] =
    mapper(Term createTerm value) match {
      case Some(v) if (accept.isDefined && accept.get(v)) || accept.isEmpty =>
        val args = if (getArgs.isDefined) getArgs.get(v) else Seq()
        if(args isEmpty) List(PrologStruct(name)) else List(PrologStruct(name, args:_*))
      case _ => List()
    }

  override def parseToInt(x: String): Option[Int] = Term createTerm x toInt

  override def parseToBoolean(x: String): Option[Boolean] = Term createTerm x toBoolean

  override def parseToCardsDistribution(x: String): Option[CardsDistribution] = (Term createTerm x) toList match {
    case Some(info) if info.size >= 3 => (info.head.toInt, info(1).toInt, info(2).toInt) match {
      case (Some(h), Some(d), Some(f)) => Some((h,d,f))
      case _ => None
    }
    case _ => None
  }

  override def parseToBriscolaSetting(x: String): Option[BriscolaSetting] = Term createTerm x toBriscolaSetting

  override def parseToPointsConversion(x: String): Option[PointsConversion] = (Term createTerm x) toList match {
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

  override def parseToCard(x: String): Option[Card] = Term createTerm x toCard

  override def parseToCardsHierarcyAndPoints(x: String): Option[CardsHierarchyAndPoints] = (Term createTerm x) toList match {
    case Some(values) => Some(values
      .map(_.toList)
      .collect{case Some(v) if v.size >= 2 => (v.head toInt, v(1) toInt)}
      .collect{case (Some(h),Some(p)) => (h,p)}
    )
    case _ => None
  }

  private implicit def termToString(x: Term): String = x toString
  private implicit def structToString(x: Struct): String = x toString
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