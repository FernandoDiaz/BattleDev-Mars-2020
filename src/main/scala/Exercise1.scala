/*******
 * Solution Scala exercice 1 BattleDev Mars 2020 
 * 
 * ***/
object Exercise1 {

  def main(args: Array[String]): Unit = {
    println(io.Source.stdin.getLines.toList.tail.groupBy(identity).mapValues(_.size).toList.sortBy(-_._2).take(2).map(_._1).mkString(" "))
  }
}
