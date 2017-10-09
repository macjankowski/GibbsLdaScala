package pl.mjankowski

import java.io.{BufferedWriter, FileWriter}

import au.com.bytecode.opencsv.CSVWriter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import collection.JavaConverters._
import scala.util.Random

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */

case class Line(label: String, text: String)

case class NumericLine(label: String, document: Array[Int])

object NlpUtils {

  def loadData(path: String, skipHeader: Boolean = true): Iterator[Line] = {

    val fileContents = Source.fromFile(path)
    val textLines = if (skipHeader) fileContents.getLines.drop(1) else fileContents.getLines

    val lines = textLines.map(s => s.split(",")).map(s => Line(s(0), s(1)))
    lines
  }

  def preprocess(data: Iterator[Line]): Array[Line] = {

    def processLine(text: String): String = {
      text.replaceAll("\\p{P}", "")
        .replaceAll("[^\\x00-\\x7F]", "")
        .toLowerCase()
    }

    data.map(s => Line(s.label, processLine(s.text))).toArray
  }

  def addEmptySymbolAdTheBeginning(data: Iterator[Line]): Array[Line] = {
    data.map(s => Line(s.label, "<s>"+s.text)).toArray
  }

  def toNumeric(data: Array[Line]): (Array[NumericLine], Int, Map[Int, String]) = {

    val dict = mutable.Set[String]()


    data.foreach(l => dict ++= l.text.split(" "))

    val wordToIndex = Random.shuffle(dict.toList).zipWithIndex.toMap

    def processSentence(sent: Array[String]): Array[Int] = sent.map(w => wordToIndex(w))

    def processLine(line: Line): NumericLine = {
      NumericLine(line.label, processSentence(line.text.split(" ")))
    }

    val numericData = data.map(line => processLine(line))
    val dictSize = dict.size

    (numericData, dictSize, wordToIndex.map(_.swap))
  }

  def forInference(data: Array[NumericLine]): Array[Array[Int]] = data.map(line => line.document)

  def saveAsCsv(listOfRecords: ListBuffer[Array[String]], path: String) = {

    val outputFile = new BufferedWriter(new FileWriter(path))
    val csvWriter = new CSVWriter(outputFile)
    val csvSchema = Array("label", "text")

    csvWriter.writeAll((csvSchema :: listOfRecords.toList).asJava)
    outputFile.close()
  }

}
