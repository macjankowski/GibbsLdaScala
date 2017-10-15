package pl.mjankowski.inference.unigrams

import pl.mjankowski.inference.InputData

/**
  *
  * @author Maciej Jankowski <mjankowski@pl.imshealth.com>
  */
object UnigramsStatistics {

  val r = new scala.util.Random(12345)

  def create(in: InputData): UnigramsStatistics = {

    var iter = 0

    val M = in.M

    var d = 0
    var i = 0

    val z: Array[Array[Int]] = new Array[Array[Int]](M)
    val topicsInDocs = Array.ofDim[Int](M, in.K)
    val sumOfTopicsInDocs = Array.fill(M)(0)

    val wordsInTopics = Array.ofDim[Int](in.K, in.V)
    val sumOfWordsInTopic = Array.fill(in.K)(0)

    while (d < M) {
      val tokenCount = in.data(d).length
      z(d) = new Array[Int](tokenCount)
      i = 0
      while (i < tokenCount) {
        val word = in.data(d)(i)
        val k = r.nextInt(in.K)
        z(d)(i) = k
        topicsInDocs(d)(k) += 1
        sumOfTopicsInDocs(d) += 1

        wordsInTopics(k)(word) += 1
        sumOfWordsInTopic(k) += 1

        i += 1
        iter += 1
      }

      val countOfWordsInDocument = in.data(d).length

      //remove after testing
      require(sumOfTopicsInDocs(d) == countOfWordsInDocument)

      d += 1
    }

    UnigramsStatistics(
      z = z,
      topicsInDocs = topicsInDocs,
      sumOfTopicsInDocs = sumOfTopicsInDocs,
      wordsInTopics = wordsInTopics,
      sumOfWordsInTopic = sumOfWordsInTopic
    )

  }
}

case class UnigramsStatistics(
                                  z: Array[Array[Int]],
                                  topicsInDocs: Array[Array[Int]],
                                  sumOfTopicsInDocs: Array[Int],
                                  wordsInTopics: Array[Array[Int]],
                                  sumOfWordsInTopic: Array[Int]){

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
}
