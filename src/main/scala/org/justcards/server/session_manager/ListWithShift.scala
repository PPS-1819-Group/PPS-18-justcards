package org.justcards.server.session_manager

/**
 * Improve a classic list with the ability to shift its elements.
 */
object ListWithShift {

  implicit class ShiftableList[A](list: List[A]) {

    /**
     * Shift to the given element
     * @param elem the element that has to be the first one
     * @return the new list
     */
    def shiftTo(elem: A): Option[List[A]] = shift(list,elem)

    private def shift(list: List[A], elem: A): Option[List[A]] = {
      @scala.annotation.tailrec
      def _shift(list: List[A])(last: A): Option[List[A]] = list.head match {
        case `elem` => Some(list)
        case e if last == e => None
        case a => _shift(list.tail :+ a)(last)
      }
      _shift(list)(list.last)
    }

  }
}
