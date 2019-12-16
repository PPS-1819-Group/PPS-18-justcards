package org.justcards.server.knowledge_engine.game_knowledge

import org.justcards.commons.{Card, GameId}
import org.justcards.commons.games_rules.BriscolaSetting.BriscolaSetting
import org.justcards.commons.games_rules.knowledge.RuleKnowledge
import org.justcards.server.Commons.Team.Team
import org.justcards.server.Commons.UserInfo

/**
 * Trait for classes that have to represent a knowledge on a specific game.
 */
trait GameKnowledge {
  type CardsNumber = Int
  type Points = Int
  type Seed = String

  /**
   * Retrieve the initial configuration of a game.
   * @return (number of cards in hand, number of cards to draw, number of cards on the field)
   */
  def initialConfiguration: (CardsNumber,CardsNumber,CardsNumber)

  /**
   * Retrieve the deck of cards to use in the game.
   * @return the deck of cards
   */
  def deckCards: Set[Card]

  /**
   * Retrieve all the available seeds to use in the game.
   * @return the available seeds.
   */
  def seeds: Set[Seed]

  /**
   * Retrieve how the Briscola should be chosen in the game.
   * @return how to chose the Briscola
   */
  def hasToChooseBriscola: BriscolaSetting

  /**
   * Set a new seed as Briscola.
   * @param seed the seed
   * @return if the Briscola is valid or not
   */
  def setBriscola(seed: Seed): Boolean

  /**
   * Determine if a card can be played or not.
   * @param card the card to be played
   * @param fieldCards the cards on the field
   * @param handCards the cards in the hand of the player playing the card
   * @return The cards on the field after the requested card was played, None otherwise
   */
  def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]]

  /**
   * Determine who won the current hand.
   * @param fieldCards the cards on the field
   * @return the winner
   */
  def handWinner(fieldCards: List[(Card,UserInfo)]): UserInfo

  /**
   * Determine the winner team of the match.
   * @param firstTeamCards the first team cards
   * @param secondTeamCards the second team cards
   * @param lastHandWinner the team who won the last hand
   * @return (the winner team, points of the first team for the session, points of the second team for the session)
   */
  def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team,Points,Points)

  /**
   * Determine the winner team of the session.
   * @param firstTeamPoints the first team points gained in the session
   * @param secondTeamPoints the second team points gained in the session
   * @return the winner team if present, None otherwise.
   */
  def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Option[Team]

  /**
   * Determine the points obtained by the teams in a match.
   * @param firstTeamCards the first team cards
   * @param secondTeamCards the second team cards
   * @param lastHandWinner the team who won the last hand
   * @return (points of the first team, points of the second team)
   */
  def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points,Points)

  /**
   * Determine the user that has to play the first turn
   * in the first match of a  the session.
   * @param playersHandCards the players and their hand cards
   * @return the user that has has to play the first turn in the first match of a the session if present, None otherwise.
   */
  def sessionStarterPlayer(playersHandCards: Set[(UserInfo, Set[Card])]): Option[UserInfo]
}

object GameKnowledge {

  type GameKnowledgeFactory = GameId => GameKnowledge

  def apply(): GameKnowledgeFactory = PrologGameKnowledge()

  val COMMON_RULES: String = RuleKnowledge.RULES_PATH concat "commonRules.pl"
  val GAMES_PATH: String = "games/"
}
