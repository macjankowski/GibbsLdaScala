package pl.mjankowski

import org.scalatest.{FunSuite, Matchers}
import pl.mjankowski.inference._
import pl.mjankowski.inference.bigrams.{GibbsSamplingBigrams, Hyperparameters, OutputData, Statistics}
import pl.mjankowski.inference.unigrams.{GibbsUnigramsEstimator, ParametersUnigrams, UnigramsStatistics}

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
class TestLda extends FunSuite with Matchers {

  //  val filePath = "/Users/mjankowski/doc/workspace/lda_gibbs_R/apps_desc_train_ASCII.csv"
  val filePath = "/Users/mjankowski/doc/workspace/data/reducedData.csv"
  val K = 10

  val estimatorUnigrams: GibbsUnigramsEstimator = new GibbsUnigramsEstimator
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

    val in = InputData(data = labelless,
      V = dictSize,
      K = 10,
      M = labelless.length,
      dict = dict)

    val stats = UnigramsStatistics.create(in = in)

    val allWordsCount = labelless.map(l => l.length).sum

    // CONDITIONS CHECKINGS

    stats.wordsInTopics.map(l => l.sum).sum should be(allWordsCount)

    stats.topicsInDocs.map(d => d.sum).sum should be(allWordsCount)

    stats.sumOfTopicsInDocs.sum should be(allWordsCount)

    stats.sumOfWordsInTopic.sum should be(allWordsCount)

  }



  test("Init for bigrams method should correctly initialize parameters") {

    val in = InputData(data = labelless,
      V = dictSize,
      K = 10,
      M = labelless.length,
      dict = dict)

    val stats = Statistics.create(in)
//    println(s"gibbsIterations = ${stats.gibbsIterations}")

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

      val alphaInit = (0 until K).map(i => 50d / K).toArray

      val algParams = AlgorithmParameters(
        burnDownPeriod = 10,
        lag = 1,
        noSamples = 1,
        noSamplesForAlpha = 50
      )

      println(alphaInit.mkString(","))

      val in: InputData = InputData(data = labelless,
        V = dictSize,
        K = 10,
        M = labelless.length,
        dict = dict)

      val stats: Statistics = Statistics.create(in)

      val h: Hyperparameters = new Hyperparameters(
        alpha = alphaInit,
        alphaSum = alphaInit.sum,
        beta = 0.1,
        K=K
      )

      val parameters: OutputData = estimatorBigrams.inferParameters(
        inputData = in,
        algParams = algParams,
        hyperparameters = h,
        stats = stats
      )

      println(s"phi = ${parameters.phi(0)(1).mkString(",")}")
      println(s"theta = ${parameters.theta(0).mkString(",")}")
      println(s"alpha = ${h.alpha.mkString(",")}")
      println(s"likelihood = ${parameters.likelihood}")
    }
  }

}
