import scala.annotation.tailrec

@tailrec
def tailfact(x: Int, fact: Int = 1): Int = if (x == 1) fact else tailfact(x-1, fact * x)

tailfact(4)