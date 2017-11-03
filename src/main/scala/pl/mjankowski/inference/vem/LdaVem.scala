package pl.mjankowski.inference.vem

import breeze.linalg.{Axis, DenseVector, normalize, sum}
import breeze.numerics.digamma
import pl.mjankowski.inference.InputData
import breeze.linalg.functions._

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
class LdaVem {

  def inferPhiAndGammaForDocument(doc: Array[Int], K: Int, beta: Array[Array[Double]],
                                  alpha: DenseVector[Double]) = {

    val N = doc.length

    val phi = initPhi(N, K)
    var gamma: DenseVector[Double] = initGamma(N, K, alpha)
    var dist: Double = 1

    var it = 0
    while (dist > 0.001) {

      for (n <- (0 until N)) {
        for (i <- (0 until K)) {
          val w = doc(i)
          phi(n)(i) = beta(i)(w) * math.exp(digamma(gamma(i)))
        }
        phi(n) = normalize(phi(n))
      }

      val newGamma = alpha + sumRows(phi)
      dist = euclideanDistance(gamma, newGamma)
      //println(dist)
      gamma = newGamma
      it += 1
    }
  }

  def initPhi(N: Int, K: Int): Array[DenseVector[Double]] = {
    val oneOverK = 1.toDouble / K
    (0 until N).toArray.map(_ => DenseVector.fill(K) {
      oneOverK
    })
  }

  def sumRows(a: Array[DenseVector[Double]]): DenseVector[Double] =
    a.reduce{ (acc,row) => acc + row}


  def initGamma(N: Int, K: Int, alpha: DenseVector[Double]): DenseVector[Double] =
    alpha + N.toDouble / K.toDouble

}
