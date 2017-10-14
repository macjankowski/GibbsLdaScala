package pl.mjankowski

import org.scalatest.{FunSuite, Matchers}
import pl.mjankowski.UciLoader.getClass

import scala.io.Source

/**
  *
  * @author Maciej Jankowski <mjankowski@pl.imshealth.com>
  */
class TestUciLoader  extends FunSuite with Matchers {

  test("Load kos") {
    val uciData = UciLoader.readUciData("/docword.kos.txt")
    val expanded = NlpUtils.expandUciData(uciData.data)
    expanded.take(10).foreach(l => println(l.mkString(",")))
//    parsed.take(10) foreach println
  }

  test("Test file"){
    val all: Iterator[String] = Source.fromURL(getClass.getResource("/docword.kos.txt")).getLines()
    val lines = all.drop(3)

    println("Max index of word")
    val words = lines.map(l => l.split(" ")).map(a => a(1).toInt - 1).toArray
//    println(words.mkString(","))
    println(s"max = ${words.max}")
    println(s"min = ${words.min}")
  }
}
