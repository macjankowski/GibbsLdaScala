package pl.mjankowski.inference

import breeze.numerics.digamma

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
class Hyperparams(
                   alpha: Array[Double],
                   alphaSum: Double,
                   beta: Double
                 ) {

  def updateAlpha(stats: Array[InterStats], M: Int, K: Int): Array[Double] = {

    val alphaSum = alpha.sum
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
//    if(newAlpha.exists(a => a.isNaN)){
      println(s"newAlpha = ${newAlpha.mkString(",")}")
//    }
    newAlpha
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
