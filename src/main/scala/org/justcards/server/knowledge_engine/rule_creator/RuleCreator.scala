package org.justcards.server.knowledge_engine.rule_creator

import org.justcards.commons.games_rules.GameRules

trait RuleCreator {
  def create(rules: GameRules): List[String]
}

object RuleCreator {
  def apply(): RuleCreator = new PrologRuleCreator()
}
