package org.justcards.commons.helper

import java.io.{FileInputStream, InputStream}

import alice.tuprolog.{Prolog, Struct, Term, Theory, Var}
import org.justcards.commons.Card
import org.justcards.commons._

/**
 * Helper facilities to improve TuProlog quality.
 */
object TuPrologHelpers {

  import alice.tuprolog.SolveInfo

  implicit def fromStringToTerm(value: String): Term = PrologStruct(value)
  implicit def fromIntToTerm(value: Int): Term = PrologInt(value)
  implicit def fromVarToString(variable: Var): String = variable getName
  implicit def fromTraversableToPrologList(traversable: Traversable[Term]): Term = PrologStruct(traversable toArray)

  /**
   * Create a Prolog engine with the specified theories
   * @param theories the Prolog theories
   * @return the Prolog engine
   */
  def prolog(theories: InputStream*): Prolog = {
    val engine = new Prolog()
    theories foreach { f => engine addTheory PrologTheory(f)}
    engine
  }

  implicit class RichMap[K,V](map: Map[K,V]) {
    def valueOf[X](key: K)(toOption: V => Option[X]): Option[X] = map get key match {
      case Some(value) => toOption(value)
      case _ => None
    }
  }

  implicit class RichProlog(knowledge: Prolog) {

    def find[X](goal: Term, variable: Var)(termToOptionX: Term => Option[X]): Option[X] = find(goal) match {
      case Some(solution) => solution.valueOf(variable)(termToOptionX)
      case _ => None
    }

    def find(goal: Term): Option[Map[String,Term]] = knowledge solve goal match {
      case info if info.isSuccess => Some(solvedVars(info))
      case _ => None
    }

    def findAll(goal: Term): Set[Map[String,Term]] = {
      @scala.annotation.tailrec
      def _findAll(info: SolveInfo)(solutions: Set[Map[String,Term]]): Set[Map[String,Term]] = {
        if (info isSuccess) {
          val newSolutions = solutions + solvedVars(info)
          if (knowledge hasOpenAlternatives) _findAll(knowledge solveNext())(newSolutions) else newSolutions
        } else solutions
      }
      _findAll(knowledge solve goal)(Set())
    }

    def ?(goal: Term): Boolean = knowledge solve goal isSuccess

    def -(goal: Term): Option[Map[String,Term]] = find(RetractTerm(goal))

    def --(goal: Term): Set[Map[String,Term]] = findAll(RetractTerm(goal))

    def +(clauses: Term*): Unit = knowledge addTheory PrologTheory(clauses:_*)

    private[this] def solvedVars(info: SolveInfo): Map[String,Term] = {
      import scala.collection.JavaConverters._
      val solvedVars = for (v <- info.getBindingVars.asScala) yield (v getName, v getTerm)
      solvedVars filter (_ != null) toMap
    }

    private[this] object RetractTerm {
      private val retract = "retract"
      def apply(termToRetract: Term): Term = PrologStruct(retract, termToRetract)
    }
  }

  implicit class RichTerm(term: Term) {
    import alice.tuprolog.Number
    import scala.collection.JavaConverters._

    private[this] val stringDelimiter = "'"

    def toInt: Option[Int] = if (term.isInstanceOf[Number]) Some(term.toString.toInt) else term.toString.toOptionInt

    def toList: Option[List[Term]] = term match {
      case t: Struct if t.isList => Some(t.listIterator().asScala.toList)
      case t: Struct if t.isCompound => Some(structToList(t))
      case _ => None
    }

    @scala.annotation.tailrec
    private def structToList(term: Term, acc: List[Term] = List()): List[Term] = term match {
      case t: Struct if t.isCompound => structToList(t getArg 1, acc :+ (t getArg 0))
      case _ => acc :+ term
    }

    def stringValue: String = term toString match {
      case v if v.startsWith(stringDelimiter) && v.endsWith(stringDelimiter) => v.slice(1,v.length-1)
      case v => v
    }

    def toBoolean: Option[Boolean] =
      try {
        Some(term.toString.toBoolean)
      } catch {
        case _: Exception => None
      }
  }

  object PrologStruct {
    def apply(): Struct = new Struct()
    def apply(value: String): Struct = new Struct(value)
    def apply(name: String, parameters: Term*): Struct = PrologStruct(name, parameters toArray)
    def apply(name: String, parameters: Array[Term]): Struct = new Struct(name, parameters)
    def apply(h: Term, t: Term): Struct = new Struct(h,t)
    def apply(terms: Term*): Struct = PrologStruct(terms toArray)
    def apply(terms: Array[Term]): Struct = new Struct(terms)
  }

  object PrologTheory {
    def apply(clauses: Term*): Theory = new Theory(clauses.foldRight(PrologStruct())((i,acc) => PrologStruct(i,acc)))
    def apply(path: String): Theory = new Theory(new FileInputStream(path))
    def apply(inputStream: InputStream): Theory = new Theory(inputStream)
  }

  /**
   * Simple way to build tuple in TuProlog format
   */
  object PrologTuple {
    private[this] val elementSeparator = ","

    /**
     * Factory to create a tuple with the specified elements in a Term representation
     * @param values the elements
     * @return the Term representation of tuple
     */
    def apply(values: Term*): Term = Term createTerm values.map(_.toString).mkString(elementSeparator)
  }

  object PrologVar {
    private[this] val variableName = "VAR"
    def apply(name: String): Var = new Var(name)
    def apply(): Var = PrologVar(variableName)
    /**
     * Create the specified amount of Prolog variables
     * @param amount the amount of variables to create
     * @return the created variables
     */
    def apply(amount: Int): List[Var] =
      (for (variableNumber <- 0 until amount) yield PrologVar(variableName + variableNumber)).toList
  }

  object PrologInt {
    type TuPrologInt = alice.tuprolog.Int
    def apply(value: Int): TuPrologInt = new TuPrologInt(value)
  }

  object PrologBoolean {
    def apply(value: Boolean): Term = Term createTerm value.toString
  }

  object PrologOperation {
    import PrologOperator.PrologOperator

    /**
     * Binary operator for Prolog operation.
     */
    object PrologOperator extends Enumeration {
      type PrologOperator = Value
      val DIVISION = Value(nextId,"/")
      val IS = Value(nextId,"is")
    }

    /**
     * Create a Struct representing a binary Prolog operation
     * @param op the operator
     * @param operand1 the first operand
     * @param operand2 the second operand
     * @return
     */
    def apply(op: PrologOperator, operand1: Term, operand2: Term): Struct = PrologStruct(op.toString,operand1,operand2)
  }

  object PrologClause {
    private[this] val clause = ":-"
    def apply(t1: Term, t2: Term): Struct = PrologStruct(clause,t1,t2)
  }
}

object PrologExtensions {

  import TuPrologHelpers._

  implicit class PrologExtendedTerm(term: Term) {
    def toCard: Option[Card] = term toList match {
      case Some(card) => card.head.toInt match {
        case Some(number) => Some(Card(number, card(1) toString))
        case _ => None
      }
      case _ => None
    }
  }

  implicit class PrologCard(card: Card) {
    def toTerm: Term = PrologTuple(card number,card seed)
  }
}