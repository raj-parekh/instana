import models.ConnectedNode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Graph(val adjList: Map[Int, List[ConnectedNode]]) {
  private val numberOfNodes: Int = adjList.size
  private val adjMatrix: Array[Array[Int]] = prepareAdjMatrix

  /**
   * Gives average latency/cost to traverse from source to destination
   *
   * @param desiredPath path from source to destination
   * @return Either cost to traverse or NO SUCH TRACE
   */
  def getAverageLatency(desiredPath: String): Either[String, Int] = {
    val nodes: Array[Int] = desiredPath.split("-").map(_.toUpperCase.charAt(0) - 'A')

    var sum = 0
    for (i <- 0 until nodes.length - 1) {
      val cost = adjMatrix(nodes(i))(nodes(i + 1))
      if (cost == 0)
        return Left("NO SUCH TRACE")

      sum += cost
    }

    Right(sum)
  }

  def sourceToDestinationWithMaxHops(source: Char, destination: Char, maxHops: Int): Int = {
    val countMatrix = countHops(maxHops)

    (1 to maxHops).foldLeft(0) { (acc, hop) =>
      acc + countMatrix(source - 'A')(destination - 'A')(hop)
    }
  }

  def sourceToDestinationWithExactHops(source: Char, destination: Char, exactHops: Int): Int = {
    countHops(exactHops)(source - 'A')(destination - 'A')(exactHops)
  }

  def minDistanceFromSourceToDestination(src: Char, dest: Char): Int = {
    val (source, destination) = (src - 'A', dest - 'A')
    val dist: Array[Int] = Array.fill(numberOfNodes)(Int.MaxValue)
    val settled: mutable.Set[Int] = mutable.Set[Int]()
    val queue: mutable.PriorityQueue[ConnectedNode] = mutable.PriorityQueue[ConnectedNode]()(Ordering.by(node => node.cost))

    queue.enqueue(ConnectedNode(source, 0))

    while (!settled.contains(destination)) {
      val currentNode: Int = queue.dequeue().node

      if (!(settled.isEmpty && currentNode == source))
        settled.add(currentNode)

      updateDistanceFromCurrentNode(source, dist, settled, queue, currentNode)
    }

    dist(destination)
  }

  def possiblePathsLessThanGivenCost(source: Char, destination: Char, cost: Int): Either[String, Int] = {
    val results: ListBuffer[String] = ListBuffer[String]()

    possiblePathsLessThanGivenCostRec(source - 'A', destination - 'A', results, cost)

    if (results.isEmpty)
      Left("NO SUCH TRACE")
    else
      Right(results.size)
  }

  /**
   * For a given k, it prepares 3 dimensional array in bottom-up manner which provides possible number of paths for a given
   * level/hop from source to destination
   *
   * @param maxHop max level/hop
   * @return 3-d array representing possible ways from source to destination for a given hop
   */
  private def countHops(maxHop: Int): Array[Array[Array[Int]]] = {
    val count = Array.fill(numberOfNodes, numberOfNodes, maxHop + 1)(0)

    for {
      hop <- 0 to maxHop
      row <- 0 until numberOfNodes
      column <- 0 until numberOfNodes
    } {
      count(row)(column)(hop) = 0

      if (hop == 0 && row == column)
        count(row)(column)(hop) = 1
      else if (hop == 1 && adjMatrix(row)(column) != 0)
        count(row)(column)(hop) = 1
      else if (hop > 1)
        for (a <- 0 until numberOfNodes if adjMatrix(row)(a) != 0)
          count(row)(column)(hop) += count(a)(column)(hop - 1)
    }

    count
  }

  /**
   * recursive function to calculate possible paths from source for a cost less than given threshold
   */
  private def possiblePathsLessThanGivenCostRec(source: Int, destination: Int, results: ListBuffer[String], cost: Int,
                                                currentPath: ListBuffer[ConnectedNode] = ListBuffer.empty): Unit = {
    if (source == destination && currentPath.nonEmpty) {
      if (currentPath.map(_.cost).sum < cost) {
        results += currentPath.map(n => (n.node + 65).toChar).mkString("-")
      }
    }

    for (node: ConnectedNode <- adjList.getOrElse(source, Nil)) {
      if (currentPath.map(_.cost).sum < cost) {
        currentPath += node

        possiblePathsLessThanGivenCostRec(node.node, destination, results, cost, currentPath)

        currentPath -= node
      }
    }
  }

  private def updateDistanceFromCurrentNode(src: Int, dist: Array[Int], settled: mutable.Set[Int],
                                            queue: mutable.PriorityQueue[ConnectedNode], currentNode: Int): Unit = {
    for (node: ConnectedNode <- adjList.getOrElse(currentNode, Nil)) {
      if (!settled.contains(node.node)) {
        val edgeDistance = node.cost
        val newDistance = getNewDistance(src, dist, currentNode, edgeDistance)

        if (newDistance < dist(node.node))
          dist(node.node) = newDistance

        queue.enqueue(ConnectedNode(node.node, node.cost))
      }
    }
  }

  private def getNewDistance(src: Int, dist: Array[Int], currentNode: Int, edgeDistance: Int): Int = {
    if (currentNode == src)
      edgeDistance
    else
      dist(currentNode) + edgeDistance
  }

  private def prepareAdjMatrix: Array[Array[Int]] = {
    val matrix: Array[Array[Int]] = Array.fill(numberOfNodes, numberOfNodes)(0)

    for {
      i: Int <- 0 until numberOfNodes
      connections: List[ConnectedNode] <- adjList.get(i)
      connection: ConnectedNode <- connections
    } {
      matrix(i)(connection.node) = connection.cost
    }

    matrix
  }
}