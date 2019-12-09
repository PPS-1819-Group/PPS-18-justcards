package org.justcards.server.knowledge_engine.rule_creator

import alice.tuprolog.{Prolog, Term}
import org.justcards.commons.Card
import org.justcards.commons.games_rules.PointsConversion.{DIVIDE, EXACTLY, MATCH_POINTS, PointsConversion}
import org.justcards.commons.games_rules.Rule
import org.justcards.server.Commons.BriscolaSetting.{NOT_BRISCOLA, SYSTEM, USER}

class PrologRuleCreator extends RuleCreator {

  import org.justcards.commons.games_rules.Rule._
  import org.justcards.commons.helper.TuPrologHelpers._
  import org.justcards.commons.helper.TuPrologHelpers.PrologOperation.PrologOperator._

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

  override def create(rules: Map[String, Any]): List[String] = {
    val theories: List[Term] = (rules flatMap { r =>
      Rule.find(r._1) match {
        case Some(CARDS_DISTRIBUTION) => r._2 match {
            case (h:Int, d:Int, _:Int) => List(PrologStruct(startHand, PrologInt(h)), PrologStruct(draw, PrologInt(d)))
            case _ => List()
        }
        case Some(PLAY_SAME_SEED) => booleanRule(r._2,playSameSeed)
        case Some(POINTS_TO_WIN_SESSION) => r._2 match {
          case v: Int => List(PrologStruct(pointsToWinSession,PrologInt(v)))
          case _ => List()
        }
        case Some(CHOOSE_BRISCOLA) => r._2 match {
          case USER => List(PrologStruct(chooseBriscola, PrologInt(1)))
          case SYSTEM => List(PrologStruct(chooseBriscola, PrologInt(0)))
          case NOT_BRISCOLA => List(PrologStruct(chooseBriscola, PrologInt(-1)))
          case _ => List()
        }
        case Some(POINTS_OBTAINED_IN_A_MATCH) => createPointsRule(r._2,points)
        case Some(STARTER_CARD) => r._2 match {
          case Card(cardNumber,cardSeed) => List(PrologStruct(starterCard,cardNumber,cardSeed))
          case _ => List()
        }
        case Some(LAST_TAKE_WORTH_ONE_MORE_POINT) => booleanRule(r._2,lastTakeHasOneMorePoint)
        case Some(CARDS_HIERARCHY_AND_POINTS) => r._2 match {
          case h::t =>
            val list = h::t collect { case (number: Int, points: Int) => (number,points) }
            val seedVar = PrologVar()
            for (i <- list.indices; info = list(i))
              yield PrologClause(PrologStruct(name = card, info._1,seedVar,i + 1,info._2),PrologStruct(seed,seedVar))
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

  private def booleanRule(value: Any, name: String): List[Term] = value match {
      case v: Boolean if v => List(PrologStruct(name))
      case _ => List()
    }

  private def createPointsRule(value: Any, name: String): List[Term] = value match {
    case MATCH_POINTS =>
      val v = PrologVar()
      List(PrologStruct(name, v, v))
    case EXACTLY => List(PrologStruct(name, PrologVar(), value.asInstanceOf[PointsConversion].value))
    case DIVIDE =>
      val vars = PrologVar(2)
      val x = vars head
      val y = vars(1)
      List(PrologClause(PrologStruct(name, x, y), PrologOperation(IS, y, PrologOperation(DIVISION, x, value.asInstanceOf[PointsConversion].value))))
    case _ => List()
  }
}