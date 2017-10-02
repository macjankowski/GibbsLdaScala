package pl.mjankowski.inference

import breeze.linalg.DenseVector
import breeze.stats.distributions.Multinomial
import pl.mjankowski.Profiler

import scala.collection.mutable.ListBuffer

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

case class ParametersBigrams(phi: Array[Array[Array[Double]]], theta: Array[Array[Double]], likelihood: Double)

class GibbsSamplingBigrams extends Estimator{

  val r = scala.util.Random

  def inferParameters(
                       data: Array[Array[Int]],
                       V: Int,
                       K: Int,
                       M: Int,
                       burnDownPeriod: Int,
                       lag: Int,
                       noSamples: Int,
                       alpha: Double,
                       beta: Double,
                       dict: Map[Int, String]): ParametersBigrams = {

    val stats = init(data, V, K)

    val topicsInDocs = stats.topicsInDocs
    val sumOfTopicsInDocs = stats.sumOfTopicsInDocs
    val wordsForWordInTopic = stats.wordsForWordInTopic
    val sumOfWordsForWordInTopic = stats.sumOfWordsForWordInTopic
    val z = stats.z

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

    def prepareDistribution(K: Int, word: Int, previousWord: Int, alpha: Double, beta: Double, V: Int, d: Int): Array[Double] = {

      var k = 0
      val distribution: Array[Double] = new Array[Double](K)

      while (k < K) {

        val left = (wordsForWordInTopic(k)(previousWord)(word) + beta) / (sumOfWordsForWordInTopic(k)(previousWord) + V * beta)
        val right = (topicsInDocs(d)(k) + alpha) /// (sumOfTopicsInDocs(d) + K * alpha)
        distribution(k) = left * right

        k += 1
      }
      distribution
    }



    var iter = 0
    var counter = 0

    val likelihoods = ListBuffer[Double]()

    while (counter < burnDownPeriod + noSamples * lag) {
      var d: Int = 0
      var i: Int = 0

      while (d < M) {
        val tokenCount = data(d).length
        i = 1
        while (i < tokenCount) {
          val previousWord = data(d)(i-1)
          val word = data(d)(i)
          val oldTopic: Int = z(d)(i)

          decr(oldTopic, word, previousWord, d)

          val distribution = prepareDistribution(K = K, word = word, previousWord = previousWord, alpha = alpha, beta = beta, V = V, d = d)

          val mult = Multinomial(DenseVector(distribution))

          val newTopic: Int = mult.sample(1)(0)
          z(d)(i) = newTopic

          incr(newTopic, word, previousWord, d)

          i += 1
          iter += 1
        }

        d += 1
      }
      counter += 1

      if(counter > burnDownPeriod &&  counter % lag == 0) {
        println(counter)
//        likelihoods += estimateLikelihood(K = K, V = V, beta = beta, wordsForWordInTopic = wordsForWordInTopic, sumOfWordsForWordInTopic = sumOfWordsForWordInTopic)
      }
    }
    val phi = estimatePhi(K = K, V = V, wordsForWordInTopic = wordsForWordInTopic, sumOfWordsForWordInTopic = sumOfWordsForWordInTopic, beta = beta)
    val theta = estimateTheta(K = K, M = M, topicsInDocs = topicsInDocs, sumOfTopicsInDoc = sumOfTopicsInDocs, alpha = alpha)

    val likelihood: Double = Profiler.profile("Gibbs Sampler - Bigrams, likelihood") {
      estimateLikelihood(K = K, V = V, beta = beta, wordsForWordInTopic = wordsForWordInTopic,
        sumOfWordsForWordInTopic = sumOfWordsForWordInTopic, dict = dict)
    }

    println(s"iter = $iter")
    ParametersBigrams(phi=phi, theta=theta, likelihood=likelihood)
  }

  def estimatePhi(K: Int, V: Int, wordsForWordInTopic: Array[Array[Array[Int]]], beta: Double, sumOfWordsForWordInTopic: Array[Array[Int]]):
      Array[Array[Array[Double]]] = {

    val phi = Array.ofDim[Double](K, V, V)

    var k = 0
    while (k < K) {
      var pV = 0
      while (pV < V - 1) {
        var v = 1
        while(v < V){
          phi(k)(pV)(v) = (wordsForWordInTopic(k)(pV)(v) + beta) / (sumOfWordsForWordInTopic(k)(pV) + (V * beta))
          v += 1
        }
        pV += 1
      }
      k += 1
    }
    phi
  }

  def estimateLikelihood(K: Int, V: Int, beta: Double, wordsForWordInTopic: Array[Array[Array[Int]]],
                         sumOfWordsForWordInTopic: Array[Array[Int]], dict: Map[Int, String]) = {

    import breeze.numerics._

    val left: Double = K * (lgamma(V * beta) - V * lgamma(beta))

    def innerLoop(j: Int, k: Int): Double = {

      val sum = wordsForWordInTopic(k)(j).sum

      val numerator = (1 until V).map{i =>
        lgamma(wordsForWordInTopic(k)(j)(i) + beta)
      }.sum
      val denominator = lgamma(sumOfWordsForWordInTopic(k)(j) + V * beta)
      val ret = numerator - denominator
      ret
    }

    val right = for{
      j <- 1 until V
      k <- 1 until K
    } yield {
      val v = innerLoop(j,k)
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
        val previousWord = data(d)(i-1)
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
