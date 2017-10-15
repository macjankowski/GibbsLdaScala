package pl.mjankowski

import org.scalatest.{FunSuite, Matchers}
import pl.mjankowski.inference.bigrams.{GibbsSamplerBigrams, Hyperparameters, Statistics}
import pl.mjankowski.inference.{AlgorithmParameters, InputData}
import pl.mjankowski.inference.unigrams.{GibbsSamplerUnigrams, GibbsUnigramsEstimator, ParametersUnigrams, UnigramsStatistics}

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
class TestUnigrams extends FunSuite with Matchers {

  val estimatorUnigrams: GibbsUnigramsEstimator = new GibbsUnigramsEstimator


  test("Test Unigrams KOS") {
    val uciData: UciData = UciLoader.readUciData("/docword.kos.txt")
    val metadata = uciData.metadata
    val data = NlpUtils.expandUciData(uciData.data)
    val dict = UciLoader.readUciDictionary("/vocab.kos.txt")
    val K = 10

    Profiler.profile("Training LDA using Unigrams") {

      val K = 10
      val burnDownPeriod = 10
      val alphaInit = (0 until K).map(i => 50d / K).toArray

      val algParams = AlgorithmParameters(
        burnDownPeriod = burnDownPeriod,
        lag = 1,
        noSamples = 1,
        noSamplesForAlpha = 10
      )

      println(alphaInit.mkString(","))

      val in: InputData = InputData(data = data,
        V = metadata.V,
        K = K,
        M = metadata.M,
        dict = dict)

      val stats: UnigramsStatistics = UnigramsStatistics.create(in)

      val h: Hyperparameters = new Hyperparameters(
        alpha = alphaInit,
        alphaSum = alphaInit.sum,
        beta = 0.1,
        K=K
      )

      val parameters: ParametersUnigrams = estimatorUnigrams.inferParameters (
        in = in,
        algParams = algParams,
        h = h,
        stats = stats
      )

      println(s"phi = ${parameters.phi(0).mkString(",")}")
      println(s"theta = ${parameters.theta(0).mkString(",")}")
      println(s"likelihood = ${parameters.likelihood}")

      for(k <- (0 until K)){
        println(s"Topic $k\n")
        val topic = parameters.phi(k)
        val topWords = topic.zipWithIndex.sortBy(- _._1).take(10).map{case (v,k) => dict(k)}
        println(topWords.mkString(",")+"\n")
      }
    }
  }

  test("Test conditional distribution"){

    val uciData: UciData = UciLoader.readUciData("/docword.kos.txt")
    val metadata = uciData.metadata
    val data = NlpUtils.expandUciData(uciData.data)
    val dict = UciLoader.readUciDictionary("/vocab.kos.txt")
    val K = 10

    val alphaInit = (0 until K).map(i => 50d / K).toArray

    println(alphaInit.mkString(","))

    val in: InputData = InputData(data = data,
      V = metadata.V,
      K = K,
      M = metadata.M,
      dict = dict)


    val h: Hyperparameters = new Hyperparameters(
      alpha = alphaInit,
      alphaSum = alphaInit.sum,
      beta = 0.1,
      K=K
    )

    val stats = UnigramsStatistics.create(in = in)
    val distribution = GibbsSamplerUnigrams.prepareDistribution(in = in, s = stats, h = h, word = 1000, d = 150)

    distribution.sum should equal (1)
  }
}
