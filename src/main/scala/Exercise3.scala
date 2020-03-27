/*******
 * Solution Scala exercice 3 BattleDev Mars 2020 
 * 
 * ***/
object Exercise3 {

  def splitTimeToIntTuple(input: String): (Int, Int) ={
    val res = input.split(":")
    (res(0).toInt, res(1).toInt)
  }

  def timesParse(list: List[String]): Seq[(Int, (Int, Int))] = {
    list.map { elem =>
      val dayTimes = elem.split(" ")
      val day = dayTimes(0)
      val times = dayTimes(1).split("-")
      val (hh0, mm0) = splitTimeToIntTuple(times(0))
      val (hhF, mmF) = splitTimeToIntTuple(times(1))
      (day.toInt, (timeToMinutes(hh0, mm0), timeToMinutes(hhF, mmF)))
    }
  }

  def timeToMinutes(hh: Int, mm: Int): Int = {
    hh * 60 + mm
  }

  def minutesToTime(mm: Int): String = {
    f"${mm / 60}%02d:${mm % 60}%02d"
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.stdin
    val lines = source.getLines

    val timesDayStartEnd = timesParse(lines.toList.tail)

    val busyTimeMapByDay = timesDayStartEnd.groupBy(_._1).map(t => // first group by day
      (t._1, t._2.map(_._2).map(mm =>
        (mm._1 to mm._2).toSet // build sets of minutes from start to end
      ).fold(Set.empty[Int])(_ union _)) // merge sets by day
    )

    def findFirstAvailableHour(day: Int): Option[String] = {
      (480 to 1020).find(mm0 => // start time between 08:00 and 17:00
        (mm0 until mm0 + 60).forall(mm => !busyTimeMapByDay.getOrElse(day, Set.empty[Int]).contains(mm)) // 1 hour window is entirely available
      ).map(mm0OK =>
        s"$day ${minutesToTime(mm0OK)}-${minutesToTime(mm0OK + 59)}" // format datetime
      )
    }

    /**
     * Another version that tests every single day of the week, less optimal
     */
    /*val res = (1 to 5).collect(day =>
      findFirstAvailableHour(day)
    ).flatten.headOption.getOrElse("")*/

    /**
     * Used by pattern matching in partial function to get findFirstAvailableHour result
     */
    object FindFirstAvailableDateTime {
      def unapply(day: Int): Option[String] = findFirstAvailableHour(day)
    }

    val res = (1 to 5).collectFirst {
      case FindFirstAvailableDateTime(firstAvailableDateTime) =>
        firstAvailableDateTime
    }.getOrElse("")

    println(res)
  }
}
