package org.justcards.commons.games_rules.converter

import org.justcards.commons.Card
import org.justcards.commons.games_rules.PointsConversion.PointsConversion
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.commons.games_rules.Rule._

trait GameRulesConverter {
  def serialize(rules: Map[String,Any]): Map[String,String]
  def deserialize(rules: Map[String,String]): Map[String,Any]
  /*def apply(x: Int): String
  def apply(x: Boolean): String
  def apply(x: (Int,Int,Int)): String
  def apply(x: BriscolaSetting): String
  def apply(x: PointsConversion): String
  def apply(x: Card): String
  def apply(x: List[(Int,Int)]): String
  def parseToInt(x: String): Option[Int]
  def parseToBoolean(x: String): Option[Boolean]
  def parseToCardsDistribution(x: String): Option[CardsDistribution]
  def parseToBriscolaSetting(x: String): Option[BriscolaSetting]
  def parseToPointsConversion(x: String): Option[PointsConversion]
  def parseToCard(x: String): Option[Card]
  def parseToCardsHierarcyAndPoints(x: String): Option[CardsHierarchyAndPoints]*/
}

object GameRulesConverter {

  def apply(): GameRulesConverter = PrologGameRulesConverter()
/*
  implicit def intToString(v: Int)(implicit converter: GameRulesConverter): String = converter(v)
  implicit def boolToString(v: Boolean)(implicit converter: GameRulesConverter): String = converter(v)
  implicit def cardsDistributionToString(v: CardsDistribution)(implicit converter: GameRulesConverter): String = converter(v)
  implicit def briscolaSettingToString(v: BriscolaSetting)(implicit converter: GameRulesConverter): String = converter(v)
  implicit def pointsConversionToString(v: PointsConversion)(implicit converter: GameRulesConverter): String = converter(v)
  implicit def cardToString(v: Card)(implicit converter: GameRulesConverter): String = converter(v)
  implicit def cardsHierarchyAndPointsToString(v: CardsHierarchyAndPoints)(implicit converter: GameRulesConverter): String = converter(v)

  implicit def parseToInt(x: String)(implicit converter: GameRulesConverter): Option[Int] = converter parseToInt x
  implicit def parseToBoolean(x: String)(implicit converter: GameRulesConverter): Option[Boolean] = converter parseToBoolean x
  implicit def parseToCardsDistribution(x: String)(implicit converter: GameRulesConverter): Option[CardsDistribution] = converter parseToCardsDistribution x
  implicit def parseToBriscolaSetting(x: String)(implicit converter: GameRulesConverter): Option[BriscolaSetting] = converter parseToBriscolaSetting x
  implicit def parseToPointsConversion(x: String)(implicit converter: GameRulesConverter): Option[PointsConversion] = converter parseToPointsConversion x
  implicit def parseToCard(x: String)(implicit converter: GameRulesConverter): Option[Card] = converter parseToCard x
  implicit def parseToCardsHierarcyAndPoints(x: String)(implicit converter: GameRulesConverter): Option[CardsHierarchyAndPoints] = converter parseToCardsHierarcyAndPoints x
  */
}
