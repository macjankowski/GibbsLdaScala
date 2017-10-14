package pl.mjankowski.inference.bigrams

import pl.mjankowski.Profiler
import pl.mjankowski.inference.{AlgorithmParameters, Estimator, InputData}

import scala.collection.mutable.ListBuffer


/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */

class GibbsSamplingBigrams extends Estimator {


  def inferParameters(
                       inputData: InputData,
                       algParams: AlgorithmParameters,
                       hyperparameters: Hyperparameters,
                       stats: Statistics): OutputData = {

    Profiler.profile(s"Burndown period. No. iterations ${algParams.burnDownPeriod}") {

      var burnDownCounter = 0
      while (burnDownCounter < algParams.burnDownPeriod) {
        println(s"Burn down = $burnDownCounter")
        GibbsSamplerBigrams.gibbsSingleIter(in = inputData, s = stats, h = hyperparameters)
        burnDownCounter += 1
      }
      println("Finished BurnDownPeriod")
    }

    var samplingCounter = 0
    while (samplingCounter < 100) {

      val interStatsList = ListBuffer[Statistics]()
      (0 until algParams.noSamplesForAlpha).foreach { i =>
        println(s"samplingCounter = $samplingCounter, i=$i")
        GibbsSamplerBigrams.gibbsSingleIter(in = inputData, s = stats, h = hyperparameters)

        if (i % 5 == 0) {
          interStatsList.append(stats.deepCopy)
        }
      }

      val S: Array[Statistics] = interStatsList.toArray
      hyperparameters.updateAlpha(S, inputData.M, inputData.K)
      samplingCounter += 1
    }

    Parameters(in = inputData, s = stats, h = hyperparameters)
  }




}
