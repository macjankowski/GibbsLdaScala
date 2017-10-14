package pl.mjankowski.inference.unigrams

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
object  GibbsSamplerUnigrams {

  def prepareDistribution(K: Int, word: Int, alpha: Double, beta: Double, V: Int, d: Int,
                          wordsInTopics: Array[Array[Int]], sumOfWordsInTopic: Array[Int],
                          topicsInDocs: Array[Array[Int]]): Array[Double] = {

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

}
