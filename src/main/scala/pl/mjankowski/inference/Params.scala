package pl.mjankowski.inference

/**
  *
  * @author Maciej Jankowski <mjankowski@pl.imshealth.com>
  */
object Params {

  def estimatePhi(K: Int, V: Int, wordsForWordInTopic: Array[Array[Array[Int]]], beta: Double, sumOfWordsForWordInTopic: Array[Array[Int]]):
  Array[Array[Array[Double]]] = {

    val phi = Array.ofDim[Double](K, V, V)

    var k = 0
    while (k < K) {
      var pV = 0
      while (pV < V - 1) {
        var v = 1
        while (v < V) {
          phi(k)(pV)(v) = (wordsForWordInTopic(k)(pV)(v) + beta / (sumOfWordsForWordInTopic(k)(pV) + (V * beta)))
          v += 1
        }
        pV += 1
      }
      k += 1
    }
    phi
  }

  def estimateTheta(K: Int, M: Int, topicsInDocs: Array[Array[Int]],
                    alphaSum: Double, alpha: Array[Double],
                    sumOfTopicsInDoc: Array[Int]): Array[Array[Double]] = {

    val theta = Array.ofDim[Double](M, K)

    var d = 0
    while (d < M) {
      var k = 0
      while (k < K) {
        theta(d)(k) = (topicsInDocs(d)(k) + alpha(k)) / (sumOfTopicsInDoc(d) + alphaSum)
        k += 1
      }
      d += 1
    }
    theta
  }

}
