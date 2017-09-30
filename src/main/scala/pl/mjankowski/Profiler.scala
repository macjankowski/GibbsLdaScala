package pl.mjankowski

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
object Profiler{

  def profile[T](methodName: String)(op:  => T): T = {
    val start = System.currentTimeMillis()

    val ret = op
    val end = System.currentTimeMillis()
    val elapsed = end - start
    val elapsedSec = elapsed / 1000
    val elapsedMin = elapsedSec / 60
    println(s"Execution of $methodName took ${elapsed} milliseconds, ${elapsedSec} seconds, ${elapsedMin} minutes" )
    ret
  }

}
