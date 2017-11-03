package pl.mjankowski

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
class ProfilerState{
  val start = System.currentTimeMillis()

  def printSnapshot = {
    val end = System.currentTimeMillis()
    val elapsed = end - start
    val elapsedSec = elapsed / 1000
    val elapsedMin = elapsedSec / 60
    println(s"Elapsed ${elapsed} milliseconds, ${elapsedSec} seconds, ${elapsedMin} minutes" )
  }
}

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

class Profiler{

  val state = new ProfilerState

  def profile[T](methodName: String)(op: ProfilerState  => T): T = {

    val state = new ProfilerState
    val ret = op(state)
    val end = System.currentTimeMillis()
    val elapsed = end - state.start
    val elapsedSec = elapsed / 1000
    val elapsedMin = elapsedSec / 60
    println(s"Execution of $methodName took ${elapsed} milliseconds, ${elapsedSec} seconds, ${elapsedMin} minutes" )
    ret
  }

}
