package pl.mjankowski

import breeze.linalg.DenseVector
import org.scalatest.FunSuite
import pl.mjankowski.inference.vem.LdaVem

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
class VemTests extends FunSuite {

  test("init phi") {
    val ldaVem = new LdaVem

    val N = 10
    val K = 3
    val b: Array[DenseVector[Double]] = ldaVem.initPhi(N, K)

    b.foreach { row =>
      row == Array.fill(K)(1.toDouble / K)
    }

    b.foreach((a: DenseVector[Double]) => println(a.data.mkString(",")))
  }

  test("init gamma") {
    val ldaVem = new LdaVem

    val N = 10
    val K = 3
    val alpha = DenseVector.fill(K)(1.toDouble / K)
    val gamma = ldaVem.initGamma(N, K, alpha)

    val myGamma: Array[Double] = (0 until K).map(i => (N + 1).toDouble / K.toDouble).toArray

    gamma.data == myGamma
  }

  test("sum breeze rows") {

    val N = 10
    val K = 3
    val ldaVem = new LdaVem
    val b: Array[DenseVector[Double]] = Array(
      DenseVector(1, 2, 3, 4, 5),
      DenseVector(1, 2, 3, 4, 5),
      DenseVector(1, 2, 3, 4, 5),
      DenseVector(1, 1, 2, 2, 3)
    )

    val summed = ldaVem.sumRows(b).data

    summed == Array(4,7,11,14,18)
  }

  test("infer phi and gamma for a document"){

    val uciData: UciData = UciLoader.readUciData("/docword.kos.txt")
    val metadata = uciData.metadata
    val data: Array[Array[Int]] = NlpUtils.expandUciData(uciData.data)
    val dict = UciLoader.readUciDictionary("/vocab.kos.txt")
    val V = metadata.V

    val K = 10
    val ldaVem = new LdaVem

    val beta: Array[Array[Double]] = (0 until K).toArray.map{ _ =>
      (0 until V).toArray.map(_ => math.random())
    }

    val alpha = DenseVector.fill(K)(1.toDouble / K)

    new Profiler().profile("variational part") { state =>

      var i=0
      while(i < data.length){
        ldaVem.inferPhiAndGammaForDocument(doc = data(i), K = K, beta = beta, alpha = alpha)

        if(i % 100 == 0){
          println(s"$i/${data.length}")
          state.printSnapshot
        }
        i += 1
      }
    }

  }
}