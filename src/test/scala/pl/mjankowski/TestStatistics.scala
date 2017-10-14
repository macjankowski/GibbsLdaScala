package pl.mjankowski

import org.scalatest.{FunSuite, Matchers}
import pl.mjankowski.inference.InputData
import pl.mjankowski.inference.bigrams.Statistics

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
class TestStatistics extends FunSuite with Matchers {

  val uciData: UciData = UciLoader.readUciData("/docword.kos.txt")
  val metadata = uciData.metadata
  val data = NlpUtils.expandUciData(uciData.data)
  val dict: Map[Int, String] = UciLoader.readUciDictionary("/vocab.kos.txt")

  val in: InputData = InputData(data = data,
    V = metadata.V,
    K = 10,
    M = metadata.M,
    dict = dict)

  test("deep copy") {
    val stats = Statistics.create(in)

    val copy = stats.deepCopy

    (copy.topicsInDocs.deep == stats.topicsInDocs.deep) should be (true)
    (copy.wordsForWordInTopic.deep == stats.wordsForWordInTopic.deep) should be (true)
    (copy.sumOfTopicsInDocs.deep == stats.sumOfTopicsInDocs.deep) should be (true)
    (copy.sumOfWordsForWordInTopic.deep == stats.sumOfWordsForWordInTopic.deep) should be (true)
//    (copy  == stats) should be (true)

    stats.topicsInDocs(1)(1) = 100000

    (copy.topicsInDocs.deep != stats.topicsInDocs.deep) should be (true)
    (copy.wordsForWordInTopic.deep == stats.wordsForWordInTopic.deep) should be (true)
    (copy.sumOfTopicsInDocs.deep == stats.sumOfTopicsInDocs.deep) should be (true)
    (copy.sumOfWordsForWordInTopic.deep == stats.sumOfWordsForWordInTopic.deep) should be (true)
  }

  test("increment and decrement") {
    val stats = Statistics.create(in)

    val copy = stats.deepCopy

    stats.incr(newTopic = 1, word = 1, previousWord = 1, d = 1)

    copy.topicsInDocs(1)(1) += 1
    copy.sumOfTopicsInDocs(1)  += 1

    (copy.topicsInDocs.deep == stats.topicsInDocs.deep) should be (true)
    (copy.sumOfTopicsInDocs.deep == stats.sumOfTopicsInDocs.deep) should be (true)

    copy.wordsForWordInTopic(1)(1)(1) += 1
    copy.sumOfWordsForWordInTopic(1)(1) += 1

    (copy.wordsForWordInTopic.deep == stats.wordsForWordInTopic.deep) should be (true)
    (copy.sumOfWordsForWordInTopic.deep == stats.sumOfWordsForWordInTopic.deep) should be (true)


  }
}
