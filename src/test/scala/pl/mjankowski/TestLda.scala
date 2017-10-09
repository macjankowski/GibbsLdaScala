package pl.mjankowski

import org.scalatest.{FunSuite, Matchers}
import pl.mjankowski.inference.{GibbsSamplingBigrams, GibbsSamplingUnigrams, ParametersBigrams, ParametersUnigrams}

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
class TestLda extends FunSuite with Matchers {

  //  val filePath = "/Users/mjankowski/doc/workspace/lda_gibbs_R/apps_desc_train_ASCII.csv"
  val filePath = "/Users/mjankowski/doc/workspace/data/reducedData.csv"
  val K = 10

  val estimatorUnigrams: GibbsSamplingUnigrams = new GibbsSamplingUnigrams
  val estimatorBigrams: GibbsSamplingBigrams = new GibbsSamplingBigrams

  // load data
  val corpus = NlpUtils.loadData(filePath)
  //  val preprocessed = NlpUtils.preprocess(corpus)
  val preprocessed = NlpUtils.addEmptySymbolAdTheBeginning(corpus)

  val (numericData: Array[NumericLine], dictSize, dict) = NlpUtils.toNumeric(preprocessed)

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

    val stats = estimatorUnigrams.init(labelless, dictSize, K)
    println(s"gibbsIterations = ${stats.gibbsIterations}")

    val allWordsCount = labelless.map(l => l.length).sum

    // CONDITIONS CHECKINGS

    allWordsCount should be(stats.gibbsIterations)

    stats.wordsInTopics.map(l => l.sum).sum should be(allWordsCount)

    stats.topicsInDocs.map(d => d.sum).sum should be(allWordsCount)

    stats.sumOfTopicsInDocs.sum should be(allWordsCount)

    stats.sumOfWordsInTopic.sum should be(allWordsCount)

  }

  test("infer unigram topic model method") {

    Profiler.profile("Gibbs Sampler") {

      val burnDownPeriod = 1000

      val parameters: ParametersUnigrams = estimatorUnigrams.inferParameters(
        data = labelless,
        V = dictSize,
        K = 10,
        M = labelless.length,
        burnDownPeriod = burnDownPeriod,
        lag = 100,
        noSamples = 10,
        alpha = 50d / K,
        beta = 0.1,
        dict
      )

      println(s"phi = ${parameters.phi(0).mkString(",")}")
      println(s"theta = ${parameters.theta(0).mkString(",")}")
      println(s"likelihood = ${parameters.likelihood}")
    }
  }

  test("Init for bigrams method should correctly initialize parameters") {

    val stats = estimatorBigrams.init(labelless, dictSize, K)
    println(s"gibbsIterations = ${stats.gibbsIterations}")

    for{
      k <- 1 until K
      pV <- 1 until dictSize
    } yield {
      stats.wordsForWordInTopic(k)(pV).sum should be (stats.sumOfWordsForWordInTopic(k)(pV))
    }

    for{
      k <- 1 until K
    } yield {
      stats.topicsInDocs(k).sum should be (stats.sumOfTopicsInDocs(k))
    }

    val allWordsCount = labelless.map(l => l.length).sum

//    allWordsCount should be(stats.gibbsIterations)

    val sum: Int = stats.wordsForWordInTopic.map(a => a.map(b => b.sum).sum).sum

    sum should be(allWordsCount)

    stats.topicsInDocs.map(d => d.sum).sum should be(allWordsCount)

    stats.sumOfTopicsInDocs.sum should be(allWordsCount)

    stats.sumOfWordsForWordInTopic.map((l: Array[Int]) => l.sum).sum should be(allWordsCount)
  }

  test("infer bigram topic model method") {

    Profiler.profile("Gibbs Sampler - Bigrams") {

      val burnDownPeriod = 10
      val alphaInit = (0 until K).map(i => 50d / K).toArray

      println(alphaInit.mkString(","))

      val parameters: ParametersBigrams = estimatorBigrams.inferParameters(
        data = labelless,
        V = dictSize,
        K = 10,
        M = labelless.length,
        burnDownPeriod = burnDownPeriod,
        lag = 1,
        noSamples = 1,
        alphaInit = alphaInit,
        beta = 0.1,
        dict = dict
      )

      println(s"phi = ${parameters.phi(0)(1).mkString(",")}")
      println(s"theta = ${parameters.theta(0).mkString(",")}")
      println(s"alpha = ${parameters.alpha.mkString(",")}")
      println(s"likelihood = ${parameters.likelihood}")
    }
  }

}
