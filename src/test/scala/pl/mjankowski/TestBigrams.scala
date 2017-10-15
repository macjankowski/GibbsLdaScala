package pl.mjankowski

import org.scalatest.{FunSuite, Matchers}
import pl.mjankowski.inference.{AlgorithmParameters, InputData}
import pl.mjankowski.inference.bigrams._
import pl.mjankowski.inference.unigrams.{GibbsUnigramsEstimator, ParametersUnigrams}

/**
  *
  * @author Maciej Jankowski <mjankowski@pl.imshealth.com>
  */
class TestBigrams extends FunSuite with Matchers {

  val estimatorBigrams: GibbsSamplingBigrams = new GibbsSamplingBigrams


  test("Test Bigrams for Kos") {
    val uciData: UciData = UciLoader.readUciData("/docword.kos.txt")
    val metadata = uciData.metadata
    val data = NlpUtils.expandUciData(uciData.data)
    val dict = UciLoader.readUciDictionary("/vocab.kos.txt")
    val K = 10

    Profiler.profile("Gibbs Sampler - Bigrams") {

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

  test("Test conditional distribution"){

    val uciData: UciData = UciLoader.readUciData("/docword.kos.txt")
    val metadata = uciData.metadata
    val data = NlpUtils.expandUciData(uciData.data)
    val dict = UciLoader.readUciDictionary("/vocab.kos.txt")
    val K = 10

    val alphaInit = (0 until K).map(i => 50d / K).toArray

    val algParams = AlgorithmParameters(
      lag = 1,
      noSamples = 1,
      burnDownPeriod = 10,
      noSamplesForAlpha = 50
    )

    println(alphaInit.mkString(","))

    val in: InputData = InputData(data = data,
      V = metadata.V,
      K = K,
      M = metadata.M,
      dict = dict)

    val stats: Statistics = Statistics.create(in)

    val h: Hyperparameters = new Hyperparameters(
      alpha = alphaInit,
      alphaSum = alphaInit.sum,
      beta = 0.1,
      K=K
    )

    val distribution = GibbsSamplerBigrams.prepareDistribution(
      in = in,
      word = 1000,
      previousWord = 234,
      d = 150,
      s = stats,
      h = h
    )

    distribution.sum should equal (1)
  }

}

