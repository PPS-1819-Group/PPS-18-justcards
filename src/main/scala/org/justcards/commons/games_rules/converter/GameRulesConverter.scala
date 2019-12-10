package org.justcards.commons.games_rules.converter

import org.justcards.commons.games_rules.Rule

trait GameRulesConverter {
  def serialize(rules: Map[Rule.Value,Any]): Map[String,String]
  def deserialize(rules: Map[String,String]): Map[Rule.Value,Any]
}

object GameRulesConverter {
  def apply(): GameRulesConverter = PrologGameRulesConverter()
}
