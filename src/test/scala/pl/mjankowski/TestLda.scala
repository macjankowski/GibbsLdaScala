package pl.mjankowski

import org.scalatest.{FunSuite, Matchers}
import pl.mjankowski.inference.{GibbsSampling, Parameters}

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
class TestLda extends FunSuite with Matchers {

  //  val filePath = "/Users/mjankowski/doc/workspace/lda_gibbs_R/apps_desc_train_ASCII.csv"
  val filePath = "/Users/mjankowski/doc/workspace/data/reducedData.csv"
  val K = 20

  val estimator: GibbsSampling = new GibbsSampling

  // load data
  val corpus = NlpUtils.loadData(filePath)
  //  val preprocessed = NlpUtils.preprocess(corpus)

  val (numericData: Array[NumericLine], dictSize) = NlpUtils.toNumeric(corpus.toArray)

  val labelless: Array[Array[Int]] = NlpUtils.forInference(numericData)

  //  test("store preprocessed file"){
  //    val listOfRecords: ListBuffer[Array[String]] = ListBuffer[Array[String]]()
  //
  //    NlpUtils.saveAsCsv(listOfRecords, "/Users/mjankowski/doc/workspace/data/gibsTestData.csv")
  //  }


  test("An empty Set should have size 0") {
    val lda = new Lda
    lda.run(filePath)
  }

  test("Init method should correctly initialize parameters") {

    val stats = estimator.init(labelless, dictSize, K)
    println(s"gibbsIterations = ${stats.gibbsIterations}")

    val allWordsCount = labelless.map(l => l.length).sum

    // CONDITIONS CHECKINGS

    allWordsCount should be(stats.gibbsIterations)

    stats.wordsInTopics.map(l => l.sum).sum should be(allWordsCount)

    stats.topicsInDocs.map(d => d.sum).sum should be(allWordsCount)

    stats.sumOfTopicsInDocs.sum should be(allWordsCount)

    stats.sumOfWordsInTopic.sum should be(allWordsCount)

  }

  test("infer method should correctly infer parameters of a model") {

    Profiler.profile("Gibbs Sampler") {

      val burnDownPeriod = 1000

      val parameters: Parameters = estimator.inferParameters(
        data = labelless,
        V = dictSize,
        K = 10,
        M = labelless.length,
        burnDownPeriod = burnDownPeriod,
        lag = 100,
        noSamples = 10,
        alpha = 50d / K,
        beta = 0.1
      )

      println(s"phi = ${parameters.phi(0).mkString(",")}")
      println(s"theta = ${parameters.theta(0).mkString(",")}")
      println(s"likelihood = ${parameters.likelihood}")
    }
  }

}
