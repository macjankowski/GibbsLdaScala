package pl.mjankowski.inference

import breeze.linalg.DenseVector
import breeze.numerics._
import breeze.stats.distributions.Multinomial
import pl.mjankowski.Profiler

import scala.collection.mutable.ListBuffer
import pl.mjankowski.inference.Hyperparams._
import Params._


/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */

case class LdaBigramsStatistics(
                                 z: Array[Array[Int]],
                                 topicsInDocs: Array[Array[Int]],
                                 sumOfTopicsInDocs: Array[Int],
                                 wordsForWordInTopic: Array[Array[Array[Int]]], // K x V x V
                                 sumOfWordsForWordInTopic: Array[Array[Int]], // K x V
                                 gibbsIterations: Int)

case class InterStats(
                       topicsInDocs: Array[Array[Int]],
                       sumOfTopicsInDocs: Array[Int],
                       wordsForWordInTopic: Array[Array[Array[Int]]], // K x V x V
                       sumOfWordsForWordInTopic: Array[Array[Int]]) {

  def deepCopy = {
    InterStats(
      topicsInDocs = topicsInDocs.map(a => a.clone),
      sumOfTopicsInDocs = sumOfTopicsInDocs.clone(),
      wordsForWordInTopic = wordsForWordInTopic.map(a => a.clone()),
      sumOfWordsForWordInTopic = sumOfWordsForWordInTopic.clone()
    )
  }
}

case class ParametersBigrams(phi: Array[Array[Array[Double]]], theta: Array[Array[Double]], likelihood: Double, alpha: Array[Double])

class GibbsSamplingBigrams extends Estimator {

  val r = scala.util.Random

  def inferParameters(
                       data: Array[Array[Int]],
                       V: Int,
                       K: Int,
                       M: Int,
                       burnDownPeriod: Int,
                       lag: Int,
                       noSamples: Int,
                       alphaInit: Array[Double],
                       beta: Double,
                       dict: Map[Int, String]): ParametersBigrams = {

    val stats = init(data, V, K)

    val topicsInDocs = stats.topicsInDocs
    val sumOfTopicsInDocs = stats.sumOfTopicsInDocs
    val wordsForWordInTopic = stats.wordsForWordInTopic
    val sumOfWordsForWordInTopic = stats.sumOfWordsForWordInTopic
    val z = stats.z
    var alpha: Array[Double] = alphaInit
//    var beta: Array[Double] = betaInit
    var alphaSum: Double = alphaInit.sum
//    var betaSum: Double = betaInit.sum

    def decr(oldTopic: Int, word: Int, previousWord: Int, d: Int) = {
      topicsInDocs(d)(oldTopic) -= 1
      sumOfTopicsInDocs(d) -= 1

      wordsForWordInTopic(oldTopic)(previousWord)(word) -= 1
      sumOfWordsForWordInTopic(oldTopic)(previousWord) -= 1

      //      require(topicsInDocs(d)(oldTopic) >= 0)
      //      require(sumOfTopicsInDocs(oldTopic) >= 0, s"${sumOfTopicsInDocs(oldTopic)}")
      //      require(wordsInTopics(oldTopic)(word) >= 0)
      //      require(sumOfWordsInTopic(oldTopic) >= 0)
    }

    def incr(newTopic: Int, word: Int, previousWord: Int, d: Int) = {
      topicsInDocs(d)(newTopic) += 1
      sumOfTopicsInDocs(d) += 1

      wordsForWordInTopic(newTopic)(previousWord)(word) += 1
      sumOfWordsForWordInTopic(newTopic)(previousWord) += 1
    }

    def prepareDistribution(K: Int, word: Int, previousWord: Int,
                            V: Int, d: Int): Array[Double] = {

      var k = 0
      val distribution: Array[Double] = new Array[Double](K)

      while (k < K) {

        val left = (wordsForWordInTopic(k)(previousWord)(word) + beta) / (sumOfWordsForWordInTopic(k)(previousWord) + V * beta)
        val right = (topicsInDocs(d)(k) + alpha(k)) /// (sumOfTopicsInDocs(d) + K * alpha)
        distribution(k) = left * right

        if(distribution(k).isNaN){
          println("NaN")
        }

        k += 1
      }
      distribution
    }


    var iter = 0

    val likelihoods = ListBuffer[Double]()

    def gibbsSingleIter = {
      var d: Int = 0
      while (d < M) {
        val tokenCount = data(d).length
        var i: Int = 1
        while (i < tokenCount) {
          val previousWord = data(d)(i - 1)
          val word = data(d)(i)
          val oldTopic: Int = z(d)(i)

          decr(oldTopic, word, previousWord, d)

          val distribution = prepareDistribution(K = K, word = word, previousWord = previousWord, V = V, d = d)

          val mult = Multinomial(DenseVector(distribution))

          val newTopic: Int = mult.sample(1)(0)
          z(d)(i) = newTopic

          incr(newTopic, word, previousWord, d)

          i += 1
          iter += 1
        }

        d += 1
      }

    }


    var burnDownCounter = 0
    while (burnDownCounter < 1000) {
      gibbsSingleIter
      burnDownCounter += 1
    }
    println("Finished BurnDownPeriod")

    var interStatsList = ListBuffer[InterStats]()
    var samplingCounter = 0
    while (samplingCounter <  100) {

      (0 until 50).foreach { i =>
        println(s"samplingCounter = $samplingCounter, i=$i")
        gibbsSingleIter
        if(i % 5 == 0){
          interStatsList.append(InterStats(
            topicsInDocs = topicsInDocs.map(a => a.clone),
            sumOfTopicsInDocs = sumOfTopicsInDocs.clone(),
            wordsForWordInTopic = wordsForWordInTopic.map(a => a.clone()),
            sumOfWordsForWordInTopic = sumOfWordsForWordInTopic.clone()
          ))
        }
      }

      val S: Array[InterStats] = interStatsList.toArray
      alpha = nextAlpha(S, M, K, alpha)
      alphaSum = alpha.sum

      samplingCounter += 1
    }

    val phi = estimatePhi(K = K, V = V, wordsForWordInTopic = wordsForWordInTopic,
      sumOfWordsForWordInTopic = sumOfWordsForWordInTopic, beta = beta)
    val theta = estimateTheta(K = K, M = M, topicsInDocs = topicsInDocs, sumOfTopicsInDoc = sumOfTopicsInDocs,
      alpha = alpha, alphaSum = alphaSum)

    println("Calculating likelihood")
    val likelihood: Double = Profiler.profile("Gibbs Sampler - Bigrams, likelihood") {
      estimateLikelihood(K = K, V = V, beta = beta, wordsForWordInTopic = wordsForWordInTopic,
        sumOfWordsForWordInTopic = sumOfWordsForWordInTopic, dict = dict)
    }

    println(s"iter = $iter")
    ParametersBigrams(phi = phi, theta = theta, likelihood = likelihood, alpha = alpha)
  }




  def estimateLikelihood(K: Int, V: Int, beta: Double, wordsForWordInTopic: Array[Array[Array[Int]]],
                         sumOfWordsForWordInTopic: Array[Array[Int]], dict: Map[Int, String]) = {


//    val left: Double = V * K * ((lgamma(betaSum) - (0 until V).map(i => lgamma(beta(i))).sum))
    val left: Double = V * K * ((lgamma(V * beta) - V * lgamma(beta)))


    def innerLoop(j: Int, k: Int): Double = {

      val sum = wordsForWordInTopic(k)(j).sum

      val lgammas = (0 until V).map { i =>
        lgamma(wordsForWordInTopic(k)(j)(i) + beta)
      }
      val numerator = lgammas.sum
      val denominator = lgamma(sumOfWordsForWordInTopic(k)(j) + V * beta)
      val ret = numerator - denominator
      ret
    }

    val right = for {
      j <- 0 until V
      k <- 0 until K
    } yield {
      val v = innerLoop(j, k)
      v
    }

    left + right.sum
  }

  //  def estimateLikelihood(K: Int, V: Int, beta: Double, wordsForWordInTopic: Array[Array[Array[Int]]], sumOfWordsForWordInTopic: Array[Array[Int]]) = {
  //
  //    import breeze.numerics._
  //
  //    val left = K * (lgamma(V * beta) - V * lgamma(beta))
  //
  //    def innerLoop(j: Int, k: Int): Double = {
  //
  //      var v = 0
  //      var subsum = 0d
  //      while (v < V) {
  //        subsum += lgamma(wordsForWordInTopic(k)(j)(v) + beta)
  //        v += 1
  //      }
  //
  //      (subsum - lgamma(sumOfWordsForWordInTopic(k)(j) + V * beta))
  //
  //    }
  //
  //
  //    var sumOverJ = 0d
  //    var j = 0
  //    while(j < V) {
  //      var k = 0
  //      var sumOverK = 0d
  //      while (k < K) {
  //        sumOverK += innerLoop(j,k)
  //        k += 1
  //      }
  //      sumOverJ += sumOverK
  //      j+=1
  //    }
  //
  //    left + sumOverJ
  //  }

  def init(data: Array[Array[Int]], V: Int, K: Int): LdaBigramsStatistics = {

    var iter = 0

    val M = data.length

    var d = 0
    var i = 0

    val z: Array[Array[Int]] = new Array[Array[Int]](M)
    val topicsInDocs = Array.ofDim[Int](M, K)
    val sumOfTopicsInDocs = Array.fill(M)(0)

    val wordsForWordInTopic = Array.ofDim[Int](K, V, V)
    val sumOfWordsForWordInTopic = Array.fill(K, V)(0)

    while (d < M) {
      val tokenCount = data(d).length
      z(d) = new Array[Int](tokenCount)
      i = 1
      while (i < tokenCount) {
        val previousWord = data(d)(i - 1)
        val word = data(d)(i)
        val k = r.nextInt(K)
        z(d)(i) = k
        topicsInDocs(d)(k) += 1
        sumOfTopicsInDocs(d) += 1

        wordsForWordInTopic(k)(previousWord)(word) += 1
        sumOfWordsForWordInTopic(k)(previousWord) += 1

        i += 1
        iter += 1
      }

      val countOfWordsInDocument = data(d).length

      //remove after testing
      require(sumOfTopicsInDocs(d) + 1 == countOfWordsInDocument)

      d += 1
    }

    LdaBigramsStatistics(
      z = z,
      topicsInDocs = topicsInDocs,
      sumOfTopicsInDocs = sumOfTopicsInDocs,
      wordsForWordInTopic = wordsForWordInTopic,
      sumOfWordsForWordInTopic = sumOfWordsForWordInTopic,
      gibbsIterations = iter
    )

  }

}
