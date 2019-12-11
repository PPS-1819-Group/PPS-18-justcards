package org.justcards.server.session_manager

object ListWithShift {

  implicit class ShiftableList[A](list: List[A]) {

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
