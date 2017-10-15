package pl.mjankowski.inference.unigrams

import breeze.linalg.DenseVector
import breeze.stats.distributions.Multinomial
import pl.mjankowski.inference.InputData
import pl.mjankowski.inference.bigrams.{Hyperparameters, Statistics}

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
object  GibbsSamplerUnigrams {

  def prepareDistribution(in: InputData, s: UnigramsStatistics, h: Hyperparameters,
                          word: Int,  d: Int): Array[Double] = {

    var k = 0
    val distribution: Array[Double] = new Array[Double](in.K)

    while (k < in.K) {

      val left = (s.wordsInTopics(k)(word) + h.beta) / (s.sumOfWordsInTopic(k) + in.V * h.beta)
      val right = (s.topicsInDocs(d)(k) + h.alphaSum) /// (sumOfTopicsInDocs(d) + K * alpha)
      distribution(k) = left * right

      k += 1
    }
    distribution
  }

  def gibbsSingleIter(in: InputData, stats: UnigramsStatistics, h: Hyperparameters) = {
    var d: Int = 0
    var i: Int = 0

    while (d < in.M) {
      val tokenCount = in.data(d).length
      i = 0
      while (i < tokenCount) {
        val word = in.data(d)(i)
        val oldTopic: Int = stats.z(d)(i)

        stats.decr(oldTopic, word, d)

        val distribution = GibbsSamplerUnigrams.prepareDistribution(in = in, s = stats, h = h, word = word, d = d)

        val mult = Multinomial(DenseVector(distribution))

        val newTopic: Int = mult.sample(1)(0)
        stats.z(d)(i) = newTopic

        stats.incr(newTopic, word, d)

        i += 1
      }
      d += 1
    }
  }

}
