package pl.mjankowski.inference.bigrams

import breeze.numerics.digamma

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
class Hyperparameters (
                   var alpha: Array[Double],
                   var alphaSum: Double,
                   var beta: Double
                 ) {

  def updateAlpha(stats: Array[Statistics], M: Int, K: Int) = {

    def kThElem(k: Int): Double = {
      val numerator = for {
        s <- (0 until stats.length)
        d <- (0 until M)
      } yield {
        val arg = stats(s).topicsInDocs(d)(k) + alpha(k)
        val left = digamma(arg)
        val right = digamma(alpha(k))

        if(left.isNaN || right.isNaN){
          println(s"left=$left, right=$right")
        }
        left - right
      }

      val denom = for {
        s <- (0 until stats.length)
        d <- (0 until M)
      } yield {
        val argLeft = stats(s).sumOfTopicsInDocs(d) + alphaSum
        val left = digamma(argLeft)
        val right = digamma(alphaSum)

        if(left.isNaN || right.isNaN){
          println(s"left=$left, right=$right")
        }

        left - right
      }
      if(denom.sum == null){
        println(s"denom.sum=${denom.sum}")
      }
      val ret = numerator.sum / denom.sum
      if(ret.isNaN){
        println(s"ret = $ret")
      }
      ret
    }

    val newAlpha = (0 until K).map(k => alpha(k) * kThElem(k)).toArray
    println(s"newAlpha = ${newAlpha.mkString(",")}")
    alpha = newAlpha
    alphaSum = alpha.sum
  }

//  def nextBeta(stats: Array[InterStats], M: Int, K: Int, beta: Array[Double]): Array[Double] = {
//
//    val betaSum = beta.sum
//    def iThElem(i: Int): Double = {
//      val numerator = for {
//        s <- (0 until stats.length)
//        k <- (0 until K)
//      } yield {
//        digamma(stats(s).topicsInDocs(k)(i) + beta(k)) - digamma(beta(k))
//      }
//
//      val denom = for {
//        s <- (0 until stats.length)
//        d <- (0 until M)
//      } yield {
//        digamma(stats(s).sumOfTopicsInDocs(d) + alphaSum) - digamma(alphaSum)
//      }
//      numerator.sum / denom.sum
//    }
//
//    val newAlpha = (0 until K).map(k => alpha(k) * kThElem(k)).toArray
//    newAlpha
//  }
}
