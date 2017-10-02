package pl.mjankowski.inference

import breeze.linalg.DenseVector
import breeze.stats.distributions.Multinomial

import scala.collection.mutable.ListBuffer

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
case class LdaUnigramsStatistics(
                          z: Array[Array[Int]],
                          topicsInDocs: Array[Array[Int]],
                          sumOfTopicsInDocs: Array[Int],
                          wordsInTopics: Array[Array[Int]],
                          sumOfWordsInTopic: Array[Int],
                          gibbsIterations: Int)

case class ParametersUnigrams(phi: Array[Array[Double]], theta: Array[Array[Double]], likelihood: Double)


class GibbsSamplingUnigrams extends Estimator {

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
                       dict: Map[Int, String]): ParametersUnigrams = {

    val stats = init(data, V, K)

    val topicsInDocs = stats.topicsInDocs
    val sumOfTopicsInDocs = stats.sumOfTopicsInDocs
    val wordsInTopics = stats.wordsInTopics
    val sumOfWordsInTopic = stats.sumOfWordsInTopic
    val z = stats.z

    def decr(oldTopic: Int, word: Int, d: Int) = {
      topicsInDocs(d)(oldTopic) -= 1
      sumOfTopicsInDocs(d) -= 1

      wordsInTopics(oldTopic)(word) -= 1
      sumOfWordsInTopic(oldTopic) -= 1

//      require(topicsInDocs(d)(oldTopic) >= 0)
//      require(sumOfTopicsInDocs(oldTopic) >= 0, s"${sumOfTopicsInDocs(oldTopic)}")
//      require(wordsInTopics(oldTopic)(word) >= 0)
//      require(sumOfWordsInTopic(oldTopic) >= 0)
    }

    def incr(newTopic: Int, word: Int, d: Int) = {
      topicsInDocs(d)(newTopic) += 1
      sumOfTopicsInDocs(d) += 1

      wordsInTopics(newTopic)(word) += 1
      sumOfWordsInTopic(newTopic) += 1
    }

    def prepareDistribution(K: Int, word: Int, alpha: Double, beta: Double, V: Int, d: Int): Array[Double] = {

      var k = 0
      val distribution: Array[Double] = new Array[Double](K)

      while (k < K) {

        val left = (wordsInTopics(k)(word) + beta) / (sumOfWordsInTopic(k) + V * beta)
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
        i = 0
        while (i < tokenCount) {
          val word = data(d)(i)
          val oldTopic: Int = z(d)(i)

          decr(oldTopic, word, d)

          val distribution = prepareDistribution(K = K, word = word, alpha = alpha, beta = beta, V = V, d = d)

          val mult = Multinomial(DenseVector(distribution))

          val newTopic: Int = mult.sample(1)(0)
          z(d)(i) = newTopic

          incr(newTopic, word, d)

          i += 1
          iter += 1
        }

        d += 1
      }
      counter += 1

      //if(counter > burnDownPeriod &&  counter % lag == 0) {
      if(counter > burnDownPeriod && counter % lag == 0){
        println(counter)
        likelihoods += estimateLikelihood(K = K, V = V, beta = beta, wordsInTopic = wordsInTopics, sumOfWordsInTopic = sumOfWordsInTopic)
      }
    }
    val phi = estimatePhi(K = K, V = V, wordsInTopic = wordsInTopics, sumOfWordsInTopic = sumOfWordsInTopic, beta = beta)
    val theta = estimateTheta(K = K, M = M, topicsInDocs = topicsInDocs, sumOfTopicsInDoc = sumOfTopicsInDocs, alpha = alpha)
//    val lik = estimateLikelihood(K = K, V = V, beta = beta, wordsInTopic = wordsInTopics, sumOfWordsInTopic = sumOfWordsInTopic)

    likelihoods.mkString(",")

    println(s"iter = $iter")
    ParametersUnigrams(phi=phi, theta=theta, likelihood=harmonicMean(likelihoods.toList))
  }



  def init(data: Array[Array[Int]], V: Int, K: Int): LdaUnigramsStatistics = {

    var iter = 0

    val M = data.length

    var d = 0
    var i = 0

    val z: Array[Array[Int]] = new Array[Array[Int]](M)
    val topicsInDocs = Array.ofDim[Int](M, K)
    val sumOfTopicsInDocs = Array.fill(M)(0)

    val wordsInTopics = Array.ofDim[Int](K, V)
    val sumOfWordsInTopic = Array.fill(K)(0)

    while (d < M) {
      val tokenCount = data(d).length
      z(d) = new Array[Int](tokenCount)
      i = 0
      while (i < tokenCount) {
        val word = data(d)(i)
        val k = r.nextInt(K)
        z(d)(i) = k
        topicsInDocs(d)(k) += 1
        sumOfTopicsInDocs(d) += 1

        wordsInTopics(k)(word) += 1
        sumOfWordsInTopic(k) += 1

        i += 1
        iter += 1
      }

      val countOfWordsInDocument = data(d).length

      //remove after testing
      require(sumOfTopicsInDocs(d) == countOfWordsInDocument)

      d += 1
    }

    LdaUnigramsStatistics(
      z = z,
      topicsInDocs = topicsInDocs,
      sumOfTopicsInDocs = sumOfTopicsInDocs,
      wordsInTopics = wordsInTopics,
      sumOfWordsInTopic = sumOfWordsInTopic,
      gibbsIterations = iter
    )

  }

  def estimatePhi(K: Int, V: Int, wordsInTopic: Array[Array[Int]], beta: Double, sumOfWordsInTopic: Array[Int]): Array[Array[Double]] = {

    val phi = Array.ofDim[Double](K, V)

    var k = 0
    while (k < K) {
      var v = 0
      while (v < V) {
        phi(k)(v) = (wordsInTopic(k)(v) + beta) / (sumOfWordsInTopic(k) + (V * beta))
        v += 1
      }
      k += 1
    }
    phi
  }



  def estimateLikelihood(K: Int, V: Int, beta: Double, wordsInTopic: Array[Array[Int]], sumOfWordsInTopic: Array[Int]) = {

    import breeze.numerics._

    val left = K * (lgamma(V * beta) - V * lgamma(beta))

    var right = 0d
    var k = 0
    while (k < K) {
      var v = 0
      var subsum = 0d
      while (v < V) {
        subsum += lgamma(wordsInTopic(k)(v) + beta)
        v += 1
      }

      right += (subsum - lgamma(sumOfWordsInTopic(k) + V * beta))
      k += 1
    }

    left + right
  }


}
