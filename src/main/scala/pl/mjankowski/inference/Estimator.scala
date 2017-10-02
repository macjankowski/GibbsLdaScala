package pl.mjankowski.inference

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
abstract class Estimator {

  def estimateTheta(K: Int, M: Int, topicsInDocs: Array[Array[Int]], alpha: Double, sumOfTopicsInDoc: Array[Int]): Array[Array[Double]] = {

    val theta = Array.ofDim[Double](M, K)

    var d = 0
    while (d < M) {
      var k = 0
      while (k < K) {
        theta(d)(k) = (topicsInDocs(d)(k) + alpha) / (sumOfTopicsInDoc(d) + (K * alpha))
        k += 1
      }
      d += 1
    }
    theta
  }

  def harmonicMean(l: List[Double]): Double = l.size.toDouble / l.map(d => 1d/d).sum

}
