package org.justcards.commons.games_rules.converter

import org.justcards.commons.games_rules.{GameRules, GameRulesSettings, Rule}

trait GameRulesConverter {
  def serialize(rules: GameRules): GameRulesSettings
  def deserialize(rules: GameRulesSettings): GameRules
}

object GameRulesConverter {
  def apply(): GameRulesConverter = PrologGameRulesConverter()
}
