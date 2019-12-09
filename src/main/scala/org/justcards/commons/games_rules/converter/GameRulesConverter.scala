package org.justcards.commons.games_rules.converter

trait GameRulesConverter {
  def serialize(rules: Map[String,Any]): Map[String,String]
  def deserialize(rules: Map[String,String]): Map[String,Any]
}

object GameRulesConverter {

  def apply(): GameRulesConverter = PrologGameRulesConverter()
}
