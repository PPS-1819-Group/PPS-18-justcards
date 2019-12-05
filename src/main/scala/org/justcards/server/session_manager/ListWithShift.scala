package org.justcards.server.session_manager

object ListWithShift {

  implicit class ShiftableList[A](list: List[A]) {

    def shiftTo(elem: A): Option[List[A]] = shift(list,elem)

    @scala.annotation.tailrec
    private def shift(list: List[A], elem: A): Option[List[A]] = list.head match {
      case `elem` => Some(list)
      case _ if list == this.list => None
      case a => shift(list.tail :+ a, elem)
    }

  }
}
