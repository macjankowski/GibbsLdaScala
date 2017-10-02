package pl.mjankowski

import pl.mjankowski.inference.GibbsSamplingUnigrams

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */


case class LdaModel(theta: Array[Array[Double]], phi: Array[Array[Double]])

class Lda {

  val estimator: GibbsSamplingUnigrams = new GibbsSamplingUnigrams

  val K = 10

  def run(filePath: String): Unit = {
    val estimator: GibbsSamplingUnigrams = new GibbsSamplingUnigrams

    // load data
    val corpus = NlpUtils.loadData(filePath)
    val preprocessed = NlpUtils.preprocess(corpus)
    val (numericData, dictSize, dict) = NlpUtils.toNumeric(preprocessed)

    val labelless = NlpUtils.forInference(numericData)

    estimator.init(labelless, dictSize, K)


    //    val length = numericCorpus.length

    //    numericCorpus.foreach(l => println(l.document.mkString(",")))

    //println(s"\n${length}")

  }

}
