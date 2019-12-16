package org.justcards.server.knowledge_engine.rule_creator

import org.justcards.commons.games_rules.GameRules

/**
 * Trait for classes that want to create the correct rules given the general rules.
 */
trait RuleCreator {
  /**
   * Parse the given rules into the correct rules.
   * @param rules the rules
   * @return the correct rules
   */
  def create(rules: GameRules): List[String]
}

object RuleCreator {
  def apply(): RuleCreator = new PrologRuleCreator()
}
