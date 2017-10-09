package pl.mjankowski.inference

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl> 30.09.2017
  */
abstract class Estimator {

  def skip[A](l:Seq[A], n:Int) =
    l.zipWithIndex.collect {case (e,i) if ((i+1) % n) == 0 => e}

  def harmonicMean(l: List[Double]): Double = l.size.toDouble / l.map(d => 1d/d).sum

}
