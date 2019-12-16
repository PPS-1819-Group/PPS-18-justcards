package org.justcards.commons.games_rules.converter

import org.justcards.commons.games_rules.{GameRules, GameRulesSettings, Rule}

/**
 * A GameRulesConverter.
 */
trait GameRulesConverter {
  /**
   * Serialize the rules.
   * @param rules the rules
   * @return the serialized rules
   */
  def serialize(rules: GameRules): GameRulesSettings
  /**
   * Deserialize the rules.
   * @param rules the serialized rules
   * @return the rules
   */
  def deserialize(rules: GameRulesSettings): GameRules
}

object GameRulesConverter {
  /**
   * Create a GameRulesConverter.
   * @return the GameRulesConverter
   */
  def apply(): GameRulesConverter = PrologGameRulesConverter()
}
