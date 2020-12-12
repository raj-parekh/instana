package util

import models.ConnectedNode

import scala.collection.mutable
import scala.io.BufferedSource

trait FileHelper {

  /**
   * Reads csv file and updates dictionary
   * NOTE: Here the assumption is, nodes will be in increasing alphabetical order starting from a/A
   *
   * @return adjacency dictionary
   */
  def readFileAndUpdateDictionary(path: String): Map[Int, List[ConnectedNode]] = {
    val bufferedSource: BufferedSource = io.Source.fromFile(path)
    val dictionary: mutable.Map[Int, List[ConnectedNode]] = collection.mutable.Map[Int, List[ConnectedNode]]()

    for (line <- bufferedSource.getLines) {
      val array = line.split(",").map(_.trim.toUpperCase.split(""))

      for (Array(s, d, cost) <- array) {
        val source = s.charAt(0) - 'A'
        val destination = d.charAt(0) - 'A'

        dictionary += source -> (ConnectedNode(destination, cost.toInt) +: dictionary.getOrElse(source, Nil))
      }
    }

    bufferedSource.close
    dictionary.toMap
  }
}
