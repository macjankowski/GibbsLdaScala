package pl.mjankowski

import breeze.linalg.DenseVector
import breeze.stats.distributions.Multinomial
import org.scalatest.FunSuite

class SimpleTests extends FunSuite {

  test("tralalala") {

    var i = 0;
    val numOfBuckets = 3
    val sampleSize = 1000000

    val mult = Multinomial(DenseVector(0.1,0.2,0.1))

    val sample = mult.sample(sampleSize)

    val hist = Array.fill(numOfBuckets)(0)

    while(i < sampleSize){
      hist(sample(i)) += 1
      i += 1
    }

    val distribution = hist.map(e => e.toDouble / sampleSize)

    println(distribution.mkString(","))

  }

  test("xxxx") {
    println((2 + 0.1)/3)
  }

}