object Bookshelf {


  def shelfSort(text: String): List[Int] = {
    text.split("\n")(0).trim.split(" ").filter(_ != "").sortBy(identity).map(_.toInt).sortBy(identity).reverse.toList

  }

  def bookSort(text: String): List[Int] = {
    text.split("\n").tail.map(_.trim.split(" ").head.toInt).sortBy(identity).reverse.toList
  }

  def toShelf(text: String) = {
    def aux(shelves: List[Int], books: List[Int], usedShelves: List[Int]): List[Int] = {
      (shelves, books, usedShelves) match {
        case (_, Nil, _) => usedShelves
        case (s, x :: xs, u :: us) if (x <= u) => aux(s, xs, ((u - x) :: us).sortBy(identity).reverse)
        case (s :: ss, x :: xs, us) if (x <= s) => aux(ss, xs, ((s - x) :: us).sortBy(identity).reverse)
        case (_, _, _) => List[Int]()
      }

    }

    val usedShelves = aux(shelfSort(text), bookSort(text), List[Int]())
    if (usedShelves.isEmpty) "impossible" else
      usedShelves.length
  }


}

