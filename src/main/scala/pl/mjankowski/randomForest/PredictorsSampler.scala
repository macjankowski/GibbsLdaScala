package pl.mjankowski.randomForest

import breeze.linalg.DenseVector
import breeze.stats.distributions.Multinomial

/**
  *
  * @author Maciej Jankowski <mjankowski@pl.imshealth.com>
  */
class StandardPredictorsSampler {

  def sample(predictors: Array[Int], distribution: Array[Double], sampleLength: Int): Array[Int] = {
    val mult = Multinomial(DenseVector(distribution))

    val sample: IndexedSeq[Int] = mult.sample(sampleLength)
    sample.toArray
  }
}


class UniformPredictorSampler extends StandardPredictorsSampler{

  def sample(predictors: Array[Int], sampleLength: Int): Array[Int] = {
    val distribution = Array.fill[Double](sampleLength)(1.0/sampleLength.toDouble)
    sample(predictors, distribution, sampleLength)
  }

}
