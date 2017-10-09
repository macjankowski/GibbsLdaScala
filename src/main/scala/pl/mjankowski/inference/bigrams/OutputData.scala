package pl.mjankowski.inference.bigrams

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
case class OutputData(
                       phi: Array[Array[Array[Double]]],
                       theta: Array[Array[Double]],
                       likelihood: Double,
                       hyperparameters: Hyperparameters)
