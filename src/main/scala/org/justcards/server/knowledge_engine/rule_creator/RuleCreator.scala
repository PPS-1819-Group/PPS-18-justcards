package org.justcards.server.knowledge_engine.rule_creator

trait RuleCreator {
  def create(rules: Map[String,Any]): List[String]
}

object RuleCreator {
  def apply(): RuleCreator = new PrologRuleCreator()
}
