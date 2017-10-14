package pl.mjankowski.inference.bigrams

import breeze.linalg.DenseVector
import breeze.stats.distributions.Multinomial
import pl.mjankowski.inference.InputData

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
object GibbsSamplerBigrams {

  def prepareDistribution(in: InputData, word: Int, previousWord: Int,
                          d: Int, s: Statistics,
                          h: Hyperparameters): Array[Double] = {

    var k = 0
    val distribution: Array[Double] = new Array[Double](in.K)

    while (k < in.K) {

      val left = (s.wordsForWordInTopic(k)(previousWord)(word) + h.beta) /
        (s.sumOfWordsForWordInTopic(k)(previousWord) + in.V * h.beta)

      val right = (s.topicsInDocs(d)(k) + h.alpha(k)) /// (sumOfTopicsInDocs(d) + K * alpha)
      distribution(k) = left * right

      if(distribution(k).isNaN){
        println("NaN")
      }

      k += 1
    }
    distribution
  }

  def gibbsSingleIter(in: InputData, s: Statistics, h: Hyperparameters) = {
    var d: Int = 0
    while (d < in.M) {
      val tokenCount = in.data(d).length
      var i: Int = 1
      while (i < tokenCount) {
        val previousWord = in.data(d)(i - 1)
        val word = in.data(d)(i)
        val oldTopic: Int = s.z(d)(i)

        s.decr(oldTopic=oldTopic, word=word, previousWord=previousWord, d=d)

        val distribution = prepareDistribution(in = in, word = word, previousWord = previousWord, d = d,
          s = s, h = h)

        val mult = Multinomial(DenseVector(distribution))

        val newTopic: Int = mult.sample(1)(0)
        s.z(d)(i) = newTopic

        s.incr(newTopic=newTopic, word=word, previousWord=previousWord, d=d)

        i += 1
      }

      d += 1
    }

  }

}
