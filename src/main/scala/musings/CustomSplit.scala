package musings


/**
  * This bad boy came out of one of the questions our Python team asks candidates.
  * 
  * The ask is to implement the Python string split method to return a Generator (analogous 
  * to Scala's std lib `Stream`).
  * 
  * I thought it a fun exercise to implement this in Scala, and go one better by abstracting 
  * the split function into a typeclass.
  * 
  * 
  */
object CustomSplit {

  trait Splitter[F[_]] {

    def fsplit(value: String, separator: String): F[String]

  }

  object Splitter {

    implicit val streamSplitter: Splitter[Stream] = instance { (value, separator) =>

      def recSplit(part: String): Stream[String] = {
        if(part.length > 0) {
          val i = part.indexOf(separator)
          val head = if(i > -1) part.substring(0, i) else part
          val tail = part.takeRight(part.length - (head.length + separator.size))
          head #:: recSplit(tail)
        } else {
          Stream.empty
        }
      }

      recSplit(value)
      
    }
    
    def instance[F[_]](splitf: (String, String) => F[String]): Splitter[F] =
      new Splitter[F] {
        def fsplit(value: String, separator: String): F[String] = splitf(value, separator)
      }

  }


  def split[F[_]](inS: String, sep: String)(implicit splitter: Splitter[F]): F[String] = {
    splitter.fsplit(inS, sep)
  }

  def main(args: Array[String]) {
    
    val result: Stream[String] = split("foo,bar", ",")
    
    result.foreach(println(_))

  }

}