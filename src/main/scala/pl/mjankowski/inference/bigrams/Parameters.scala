package pl.mjankowski.inference.bigrams

import breeze.numerics.lgamma
import pl.mjankowski.inference.InputData

/**
  *
  * @author Maciej Jankowski <mjankowski@pl.imshealth.com>
  */
object Parameters {

  def apply(in: InputData, s: Statistics, h: Hyperparameters): OutputData = {

    val phi = estimatePhi(in = in, s = s, h = h)
    val theta = estimateTheta(in = in, s = s, h = h)
    val likelihood = estimateLikelihood(in = in, s = s, h = h)

    OutputData(
      phi = phi,
      theta = theta,
      likelihood = likelihood,
      hyperparameters = h)
  }

  def estimatePhi(in: InputData, s: Statistics, h: Hyperparameters):
  Array[Array[Array[Double]]] = {

    val phi = Array.ofDim[Double](in.K, in.V, in.V)

    var k = 0
    while (k < in.K) {
      var pV = 0
      while (pV < in.V - 1) {
        var v = 1
        while (v < in.V) {
          phi(k)(pV)(v) = (s.wordsForWordInTopic(k)(pV)(v) + h.beta / (s.sumOfWordsForWordInTopic(k)(pV) + (in.V * h.beta)))
          v += 1
        }
        pV += 1
      }
      k += 1
    }
    phi
  }

  def estimateTheta(in: InputData, s: Statistics, h: Hyperparameters): Array[Array[Double]] = {

    val theta = Array.ofDim[Double](in.M, in.K)

    var d = 0
    while (d < in.M) {
      var k = 0
      while (k < in.K) {
        theta(d)(k) = (s.topicsInDocs(d)(k) + h.alpha(k)) / (s.sumOfTopicsInDocs(d) + h.alphaSum)
        k += 1
      }
      d += 1
    }
    theta
  }

  def estimateLikelihood(in: InputData, s: Statistics, h: Hyperparameters) = {


    //    val left: Double = V * K * ((lgamma(betaSum) - (0 until V).map(i => lgamma(beta(i))).sum))
    val left: Double = in.V * in.K * ((lgamma(in.V * h.beta) - in.V * lgamma(h.beta)))


    def innerLoop(j: Int, k: Int): Double = {

      val sum = s.wordsForWordInTopic(k)(j).sum

      val lgammas = (0 until in.V).map { i =>
        lgamma(s.wordsForWordInTopic(k)(j)(i) + h.beta)
      }
      val numerator = lgammas.sum
      val denominator = lgamma(s.sumOfWordsForWordInTopic(k)(j) + in.V * h.beta)
      val ret = numerator - denominator
      ret
    }

    val right = for {
      j <- 0 until in.V
      k <- 0 until in.K
    } yield {
      val v = innerLoop(j, k)
      v
    }

    left + right.sum
  }

}
