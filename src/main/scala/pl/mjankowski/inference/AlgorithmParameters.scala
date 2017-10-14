package pl.mjankowski.inference

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
case class AlgorithmParameters(
                           burnDownPeriod: Int,
                           lag: Int,
                           noSamples: Int,
                           noSamplesForAlpha: Int
                         )