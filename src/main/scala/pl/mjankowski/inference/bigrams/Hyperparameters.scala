package pl.mjankowski.inference.bigrams

import breeze.numerics.digamma

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
class Hyperparameters(
                       var alpha: Array[Double],
                       var alphaSum: Double,
                       var beta: Double,
                       val K:Int
                     ) {

  var docSumDenom: Double = 0
  var docSumNum: Array[Double] = Array.ofDim[Double](K)

  def acceptNewAlpha = {
    val newAlpha = (0 until K).map { k =>
      alpha(k) * (docSumNum(k) / docSumDenom)
    }.toArray

    println(s"newAlpha = ${newAlpha.mkString(",")}")
    alpha = newAlpha
    alphaSum = newAlpha.sum
  }

  def updateAlpha(stats: Statistics, M: Int, K: Int) = {

    //    def kThElem(k: Int): Double = {
    //      val numerator = for {
    //        s <- (0 until stats.length)
    //        d <- (0 until M)
    //      } yield {
    //        val arg = stats(s).topicsInDocs(d)(k) + alpha(k)
    //        val left = if(arg == 0) 0 else digamma(arg)
    //        val right = if(alpha(k) == 0) 0 else digamma(alpha(k))
    //
    //        if(left.isNaN || right.isNaN){
    //          println(s"left=$left, right=$right")
    //        }
    //        left - right
    //      }
    //
    //      val denom = for {
    //        s <- (0 until stats.length)
    //        d <- (0 until M)
    //      } yield {
    //        val argLeft = stats(s).sumOfTopicsInDocs(d) + alphaSum
    //        val left = if(argLeft == 0) 0 else digamma(argLeft)
    //        val right = if(alphaSum == 0) 0 else digamma(alphaSum)
    //
    //        if(left.isNaN || right.isNaN){
    //          println(s"left=$left, right=$right")
    //        }
    //
    //        left - right
    //      }
    //      if(denom.sum == null){
    //        println(s"denom.sum=${denom.sum}")
    //      }
    //      val ret = numerator.sum / denom.sum
    //      if(ret.isNaN){
    //        println(s"ret = $ret")
    //      }
    //      ret
    //    }

    def innerSumNum(kThTopicForDocInSthIter: Int, kThAlpha: Double): Double =
      digamma(kThTopicForDocInSthIter + kThAlpha) - digamma(kThAlpha)

    def innerSumDenom(sumOfTopicsInDocsSthIter: Int, alphaSum: Double): Double =
      digamma(sumOfTopicsInDocsSthIter + alphaSum) - digamma(alphaSum)

    def sumForDocumentsNum(M: Int, s: Statistics, k: Int): Double =
      (0 until M).map(d => innerSumNum(s.topicsInDocs(d)(k), alpha(k))).sum

    def sumForDocumentsDenom(s: Statistics): Double =
      (0 until M).map(d => innerSumDenom(s.sumOfTopicsInDocs(d), alphaSum)).sum

    def sumForStatisticsNum(M: Int, stats: Array[Statistics], k: Int): Double =
      stats.map(s => sumForDocumentsNum(M, s, k)).sum

    def sumForStatisticsDenom(stats: Array[Statistics]): Double =
      stats.map(s => sumForDocumentsDenom(s)).sum

    //    val denom: Double = sumForStatisticsDenom(stats)
    //    val newAlpha = (0 until K).map { k =>
    //      alpha(k) * (sumForStatisticsNum(M, stats, k) / denom)
    //    }.toArray

    var newDocSumDenom: Double = sumForDocumentsDenom(stats)
    docSumDenom += newDocSumDenom

    val newDocSumNum: Array[Double] = (0 until K).toArray.map(k => sumForDocumentsNum(M, stats, k))
    docSumNum = newDocSumNum.zip(docSumNum).map { case (x, y) => x + y }


    //    println(s"newAlpha = ${newAlpha.mkString(",")}")
    //    alpha = newAlpha
    //    alphaSum = newAlpha.sum
  }
}
