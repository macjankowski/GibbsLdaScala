package pl.mjankowski

import org.scalatest.{FunSuite, Matchers}
import pl.mjankowski.inference.bigrams.{GibbsSamplerBigrams, Hyperparameters, Statistics}
import pl.mjankowski.inference.{AlgorithmParameters, InputData}
import pl.mjankowski.inference.unigrams.{GibbsSamplerUnigrams, GibbsSamplingUnigrams, ParametersUnigrams}

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
class TestUnigrams extends FunSuite with Matchers {

  val estimatorUnigrams: GibbsSamplingUnigrams = new GibbsSamplingUnigrams


  test("Test Unigrams") {
    val uciData: UciData = UciLoader.readUciData("/docword.kos.txt")

    val data = NlpUtils.expandUciData(uciData.data)
    val dict = UciLoader.readUciDictionary("/vocab.kos.txt")

    Profiler.profile("Gibbs Sampler") {

      val K = 10
      val burnDownPeriod = 1000

      val parameters: ParametersUnigrams = estimatorUnigrams.inferParameters (
        data = data,
        V = uciData.metadata.V,
        K = 10,
        M = uciData.metadata.M,
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
      beta = 0.1
    )

    val stats = estimatorUnigrams.init(in.data, metadata.V, K)

    val distribution = GibbsSamplerUnigrams.prepareDistribution(K = K, word = 1000, alpha = h.alphaSum,
      beta = h.beta, V = metadata.V, d = 150, wordsInTopics = stats.wordsInTopics,
      sumOfWordsInTopic = stats.sumOfWordsInTopic, topicsInDocs = stats.topicsInDocs)

    distribution.sum should equal (1)
  }
}
