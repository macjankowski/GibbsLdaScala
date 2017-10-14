package pl.mjankowski.inference.bigrams

import pl.mjankowski.inference.InputData

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
object Statistics {

  val r = new scala.util.Random(12345)

  def create(input: InputData): Statistics = {

    var iter = 0

    val M = input.data.length

    var d = 0
    var i = 0

    val z: Array[Array[Int]] = new Array[Array[Int]](M)
    val topicsInDocs = Array.ofDim[Int](M, input.K)
    val sumOfTopicsInDocs = Array.fill(M)(0)

    val wordsForWordInTopic = Array.ofDim[Int](input.K, input.V, input.V)
    val sumOfWordsForWordInTopic = Array.fill(input.K, input.V)(0)

    while (d < M) {
      val tokenCount = input.data(d).length
      z(d) = new Array[Int](tokenCount)
      i = 1
      while (i < tokenCount) {
        val previousWord = input.data(d)(i - 1)
        val word = input.data(d)(i)
        val k = r.nextInt(input.K)
        z(d)(i) = k
        topicsInDocs(d)(k) += 1
        sumOfTopicsInDocs(d) += 1

        wordsForWordInTopic(k)(previousWord)(word) += 1
        sumOfWordsForWordInTopic(k)(previousWord) += 1

        i += 1
        iter += 1
      }

      val countOfWordsInDocument = input.data(d).length

      //remove after testing
      require(sumOfTopicsInDocs(d) + 1 == countOfWordsInDocument)

      d += 1
    }

    new Statistics(
      topicsInDocs = topicsInDocs,
      sumOfTopicsInDocs = sumOfTopicsInDocs,
      wordsForWordInTopic = wordsForWordInTopic,
      sumOfWordsForWordInTopic = sumOfWordsForWordInTopic,
      z = z
    )

  }
}

class Statistics private(
                          var topicsInDocs: Array[Array[Int]],
                          var sumOfTopicsInDocs: Array[Int],
                          var wordsForWordInTopic: Array[Array[Array[Int]]], // K x V x V 10*6906*6906 = 476928360 * 4 = 2GB
                          var sumOfWordsForWordInTopic: Array[Array[Int]], // K x V
                          var z: Array[Array[Int]]
                        ) {


  def decr(oldTopic: Int, word: Int, previousWord: Int, d: Int) = {
    topicsInDocs(d)(oldTopic) -= 1
    sumOfTopicsInDocs(d) -= 1

//    require(topicsInDocs(d).sum == sumOfTopicsInDocs(d))

    wordsForWordInTopic(oldTopic)(previousWord)(word) -= 1
    sumOfWordsForWordInTopic(oldTopic)(previousWord) -= 1

//    require(wordsForWordInTopic(oldTopic)(previousWord).sum == sumOfWordsForWordInTopic(oldTopic)(previousWord))


//    require(topicsInDocs(d)(oldTopic) >= 0)
//    require(sumOfTopicsInDocs(oldTopic) >= 0, s"${sumOfTopicsInDocs(oldTopic)}")
//    require(wordsInTopics(oldTopic)(word) >= 0)
//    require(sumOfWordsInTopic(oldTopic) >= 0)
  }

  def incr(newTopic: Int, word: Int, previousWord: Int, d: Int) = {
    topicsInDocs(d)(newTopic) += 1
    sumOfTopicsInDocs(d) += 1

//    require(topicsInDocs(d).sum == sumOfTopicsInDocs(d))

    wordsForWordInTopic(newTopic)(previousWord)(word) += 1
    sumOfWordsForWordInTopic(newTopic)(previousWord) += 1

//    require(wordsForWordInTopic(newTopic)(previousWord).sum == sumOfWordsForWordInTopic(newTopic)(previousWord))
  }

  def deepCopy: Statistics = {
    new Statistics(
      topicsInDocs = topicsInDocs.map(a => a.clone),
      sumOfTopicsInDocs = sumOfTopicsInDocs.clone(),
      wordsForWordInTopic = wordsForWordInTopic.map(a => a.map(ia => ia.clone())),
      sumOfWordsForWordInTopic = sumOfWordsForWordInTopic.map(a => a.clone),
      z = z
    )
  }


  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: Statistics =>
        this.sumOfTopicsInDocs.deep == that.sumOfTopicsInDocs.deep &&
          this.topicsInDocs.deep == that.topicsInDocs &&
          this.sumOfWordsForWordInTopic.deep == that.sumOfWordsForWordInTopic.deep &&
          this.wordsForWordInTopic == that.wordsForWordInTopic.deep
      case _ => false
    }
  }
}
