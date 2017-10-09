package pl.mjankowski.inference

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */
case class InputData(
                 K: Int,
                 V: Int,
                 M: Int,
                 data: Array[Array[Int]],
                 dict: Map[Int, String]
               )
