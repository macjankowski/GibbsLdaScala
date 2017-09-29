package pl.mjankowski

import pl.mjankowski.inference.GibbsSampling

/**
  *
  */


case class LdaModel(theta: Array[Array[Double]], phi: Array[Array[Double]])

class Lda {

  val estimator: GibbsSampling = new GibbsSampling

  val K = 10

  def run(filePath: String): Unit = {
    val estimator: GibbsSampling = new GibbsSampling

    // load data
    val corpus = NlpUtils.loadData(filePath)
    val preprocessed = NlpUtils.preprocess(corpus)
    val (numericData, dictSize) = NlpUtils.toNumeric(preprocessed)

    val labelless = NlpUtils.forInference(numericData)

    estimator.init(labelless, dictSize, K)


    //    val length = numericCorpus.length

    //    numericCorpus.foreach(l => println(l.document.mkString(",")))

    //println(s"\n${length}")

  }

}
