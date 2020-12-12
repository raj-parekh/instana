import models.ConnectedNode
import util.FileHelper

object DistributedTracing extends App with FileHelper {

  private val adjList: Map[Int, List[ConnectedNode]] = readFileAndUpdateDictionary("src/main/resources/test.csv")
  private val graph = new Graph(adjList)

  println(graph.getAverageLatency("A-B-C"))
  println(graph.getAverageLatency("A-D"))
  println(graph.getAverageLatency("A-D-C"))
  println(graph.getAverageLatency("A-E-B-C-D"))
  println(graph.getAverageLatency("A-E-D"))
  println(graph.sourceToDestinationWithMaxHops('C', 'C', 3))
  println(graph.sourceToDestinationWithExactHops('A', 'C', 4))
  println(graph.minDistanceFromSourceToDestination('B', 'B'))
  println(graph.minDistanceFromSourceToDestination('A', 'C'))
  println(graph.possiblePathsLessThanGivenCost('C', 'C', 30))
}