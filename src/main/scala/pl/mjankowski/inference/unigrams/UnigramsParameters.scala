package pl.mjankowski.inference.unigrams

import pl.mjankowski.inference.InputData
import pl.mjankowski.inference.bigrams.Hyperparameters

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
object UnigramsParameters {

  def estimatePhi(in: InputData, s: UnigramsStatistics, h: Hyperparameters): Array[Array[Double]] = {

    val phi = Array.ofDim[Double](in.K, in.V)

    var k = 0
    while (k < in.K) {
      var v = 0
      while (v < in.V) {
        phi(k)(v) = (s.wordsInTopics(k)(v) + h.beta) / (s.sumOfWordsInTopic(k) + (in.V * h.beta))
        v += 1
      }
      k += 1
    }
    phi
  }

  def estimateLikelihood(in: InputData, s: UnigramsStatistics, h: Hyperparameters) = {

    import breeze.numerics._

    val left = in.K * (lgamma(in.V * h.beta) - in.V * lgamma(h.beta))

    var right = 0d
    var k = 0
    while (k < in.K) {
      var v = 0
      var subsum = 0d
      while (v < in.V) {
        subsum += lgamma(s.wordsInTopics(k)(v) + h.beta)
        v += 1
      }

      right += (subsum - lgamma(s.sumOfWordsInTopic(k) + in.V * h.beta))
      k += 1
    }

    left + right
  }

  def estimateTheta(in: InputData, s: UnigramsStatistics, h: Hyperparameters): Array[Array[Double]] = {

    val theta = Array.ofDim[Double](in.M, in.K)

    var d = 0
    while (d < in.M) {
      var k = 0
      while (k < in.K) {
        theta(d)(k) = (s.topicsInDocs(d)(k) + h.alphaSum) / (s.sumOfTopicsInDocs(d) + (in.K * h.alphaSum))
        k += 1
      }
      d += 1
    }
    theta
  }
}
