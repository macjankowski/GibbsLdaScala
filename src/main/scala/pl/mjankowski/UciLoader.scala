package pl.mjankowski

import scala.collection.immutable
import scala.io.Source

/**
  *
  * @author Maciej Jankowski <mjankowski@pl.imshealth.com>
  */
case class UciData(metadata: UciMetadata, data: Array[UciLine])

case class UciMetadata(M: Int, V: Int, NNZ: Int)

case class UciLine(docId: Int, wordId: Int, wordCount: Int)

object UciLoader {

  def readUciDictionary(path: String): Map[Int, String] = {
    val all: Iterator[String] = Source.fromURL(getClass.getResource(path)).getLines()
    all.zipWithIndex.map(_.swap).toMap
  }

  def readUciData(path: String): UciData = {
    val all: Iterator[String] = Source.fromURL(getClass.getResource(path)).getLines()

    val M = all.next().toInt
    val V = all.next().toInt
    val NNZ = all.next().toInt

    println(s"No. documents $M")
    println(s"No. words $V")
    println(s"No. nnz $NNZ")

    val metadata = UciMetadata(M, V, NNZ)

    val parsed: Array[UciLine] = all.map(l => l.split(" ")).map(a => UciLine(a(0).toInt, a(1).toInt - 1, a(2).toInt)).toArray
    UciData(metadata = metadata, data = parsed)
  }


}
