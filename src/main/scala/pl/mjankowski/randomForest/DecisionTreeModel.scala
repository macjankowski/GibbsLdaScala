package pl.mjankowski.randomForest

/**
  *
  * @author Maciej Jankowski <maciej.jankowski@wat.edu.pl>
  */

sealed trait Tree

private case class Node(
                         predictorIndex: Int,
                         left: Tree,
                         right: Tree,
                         split: Double) extends Tree

private case class Leaf(label: Int) extends Tree


class DecisionTreeModel(val root: Tree) {

  def predictSingleItem(item: Array[Int], startNode: Tree): Int = {

    root match {
      case Leaf(label) => label
      case Node(predictorIndex, left, right, split) =>
        val value = item(predictorIndex)
        if(value < split)
          predictSingleItem(item, left)
        else
          predictSingleItem(item, right)
    }
  }

  def predict(data: Array[Array[Int]]): Array[Int] =
    data.map(arr => predictSingleItem(arr, root))

}


