package org.justcards.server.knowledge_engine.rule_creator

import alice.tuprolog.{Prolog, Term}
import org.justcards.commons.Card
import org.justcards.commons.games_rules.PointsConversionType.{DIVIDE, EXACTLY, MATCH_POINTS}
import org.justcards.commons.games_rules.{GameRules, PointsConversion}
import org.justcards.commons.games_rules.BriscolaSetting.{BriscolaSetting, NOT_BRISCOLA, SYSTEM, USER}

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
  private val ruleEndChar = "."
  private val ruleEndPattern = "\\" + ruleEndChar
  private val space = " "

  override def create(rules: GameRules): List[String] = {
    val theories: List[Term] = (rules flatMap {
      case (CARDS_DISTRIBUTION, (h:Int, d:Int, _:Int)) => List(PrologStruct(startHand, PrologInt(h)), PrologStruct(draw, PrologInt(d)))
      case (PLAY_SAME_SEED, v: Boolean) if v => List(PrologStruct(playSameSeed))
      case (POINTS_TO_WIN_SESSION, v: Int) => List(PrologStruct(pointsToWinSession,PrologInt(v)))
      case (CHOOSE_BRISCOLA,v: BriscolaSetting) => v match {
        case USER => List(PrologStruct(chooseBriscola, PrologInt(1)))
        case SYSTEM => List(PrologStruct(chooseBriscola, PrologInt(0)))
        case NOT_BRISCOLA => List(PrologStruct(chooseBriscola, PrologInt(-1)))
        case _ => List()
      }
      case (POINTS_OBTAINED_IN_A_MATCH,v: PointsConversion) => createPointsRule(v,points)
      case (WINNER_POINTS,v: PointsConversion) => createPointsRule(v,winnerPoints)
      case (LOSER_POINTS,v: PointsConversion) => createPointsRule(v,loserPoints)
      case (DRAW_POINTS,v: PointsConversion) => createPointsRule(v,drawPoints)
      case (STARTER_CARD,c: Card) => List(PrologStruct(starterCard,c number,c seed))
      case (LAST_TAKE_WORTH_ONE_MORE_POINT,v: Boolean) if v => List(PrologStruct(lastTakeHasOneMorePoint))
      case (CARDS_HIERARCHY_AND_POINTS,v: List[Any]) =>
        val list = v collect { case (number: Int, points: Int) => (number,points) }
        val seedVar = PrologVar()
        for (i <- list.indices; info = list(i))
          yield PrologClause(PrologStruct(name = card, info._1,seedVar,i + 1,info._2),PrologStruct(seed,seedVar))
      case _ => List()
    }).toList
    val prolog = new Prolog()
    prolog.+(theories: _*)
    prolog.getTheory.toString.split(ruleEndPattern) collect {
      case r if !r.isBlank => r.trim.replaceAll(newLinePattern,space) + ruleEndChar
    } toList
  }

  private def createPointsRule(conversion: PointsConversion, name: String): List[Term] = conversion.typology match {
    case MATCH_POINTS =>
      val v = PrologVar()
      List(PrologStruct(name, v, v))
    case EXACTLY => List(PrologStruct(name, PrologVar(), conversion value))
    case DIVIDE =>
      val vars = PrologVar(2)
      val x = vars head
      val y = vars(1)
      List(PrologClause(PrologStruct(name, x, y), PrologOperation(IS, y, PrologOperation(DIVISION, x, conversion value))))
    case _ => List()
  }
}