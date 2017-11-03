package pl.mjankowski

import breeze.linalg.DenseVector
import breeze.numerics.digamma
import breeze.stats.distributions.Multinomial
import org.scalatest.FunSuite

import scala.collection.immutable

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
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

  test("arrays"){

    val a = Array.ofDim[Int](10, 1000, 1000)
    a(2)(10)(5) += 1

    println(a(2)(10)(5))

  }

  test("flatMap"){

    val V = 5
    val K = 10

    val right = for{
      j <- 1 until V
      k <- 1 until K
    } yield {
      (j,k)
    }

    println(right.mkString(","))

  }

  test("digamma"){
    for(i <- (0 until 10)){
      println(s"digamma($i) = ${digamma(i)}")
    }

  }

  test("digamma"){
    val l = List(List(1,2), List(3,5), List(3,2,6))
    val l2: immutable.Seq[Int] = l.flatten
  }


}