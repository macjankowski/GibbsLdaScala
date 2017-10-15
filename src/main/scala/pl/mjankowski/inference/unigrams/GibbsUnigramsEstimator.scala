package pl.mjankowski.inference.unigrams

import breeze.linalg.DenseVector
import breeze.stats.distributions.Multinomial
import pl.mjankowski.inference.bigrams.Hyperparameters
import pl.mjankowski.inference.{AlgorithmParameters, Estimator, InputData}

import scala.collection.mutable.ListBuffer

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */


case class ParametersUnigrams(phi: Array[Array[Double]], theta: Array[Array[Double]], likelihood: Double)


class GibbsUnigramsEstimator extends Estimator {

  val r = scala.util.Random

  def inferParameters(
                       in: InputData,
                       algParams: AlgorithmParameters,
                       h: Hyperparameters,
                       stats: UnigramsStatistics): ParametersUnigrams = {

    val likelihoods = ListBuffer[Double]()

    var burnDownCounter = 0
    while (burnDownCounter < algParams.burnDownPeriod){
      GibbsSamplerUnigrams.gibbsSingleIter(in = in, stats = stats, h = h)
      burnDownCounter += 1
    }

    var sampleCounter = 0
    while (sampleCounter < algParams.noSamples * algParams.lag) {
      println(s"cunter = $sampleCounter")

      //if(counter > burnDownPeriod &&  counter % lag == 0) {
      if(sampleCounter % 10 == 0){
        println(sampleCounter)
        likelihoods += UnigramsParameters.estimateLikelihood(in = in, s = stats, h = h)
      }
      sampleCounter += 1
    }

    val phi = UnigramsParameters.estimatePhi(in = in, s = stats, h = h)
    val theta = UnigramsParameters.estimateTheta(in = in, s = stats, h = h)

    ParametersUnigrams(phi=phi, theta=theta, likelihood=harmonicMean(likelihoods.toList))
  }

}
