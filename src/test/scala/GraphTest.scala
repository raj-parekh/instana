import org.scalatest.FunSpec
import util.FileHelper

class GraphTest extends FunSpec with FileHelper {
  private val graph: Graph = new Graph(readFileAndUpdateDictionary("src/test/resources/test.csv"))

  describe("Average Latency") {
    it("should give avg. cost of 9 from A-B-C") {
      assert(graph.getAverageLatency("A-B-C") == Right(9))
    }

    it("should give avg. cost of 5 from A-D") {
      assert(graph.getAverageLatency("A-D") == Right(5))
    }

    it("should give avg. cost of 13 from A-D-C") {
      assert(graph.getAverageLatency("A-D-C") == Right(13))
    }

    it("should give avg. cost of 22 from A-E-B-C-D") {
      assert(graph.getAverageLatency("A-E-B-C-D") == Right(22))
    }

    it("should give NO SUCH TRACE from A-E-D") {
      assert(graph.getAverageLatency("A-E-D") == Left("NO SUCH TRACE"))
    }
  }

  describe("Source to destination with max/exact hops") {
    it("should give 2 from source C to destination C with max 3 hops") {
      assert(graph.sourceToDestinationWithMaxHops('C', 'C', 3) == 2)
    }

    it("should give 3 from source A to destination C with exact 4 hops") {
      assert(graph.sourceToDestinationWithExactHops('A', 'C', 4) == 3)
    }
  }

  describe("Min distance from source to destination") {
    it("should give 9 as min distance from A to C") {
      assert(graph.minDistanceFromSourceToDestination('A', 'C') == 9)
    }

    it("should give 9 as min distance from B to B") {
      assert(graph.minDistanceFromSourceToDestination('B', 'B') == 9)
    }
  }

  describe("Different traces/paths from source to destination with the cost less than given threshold") {
    it("should give 7 for source C to destination C with the cost less than 30") {
      assert(graph.possiblePathsLessThanGivenCost('C', 'C', 30) == Right(7))
    }

    it("should give NO SUCH TRACE for source D to destination A with the cost less than 9") {
      assert(graph.possiblePathsLessThanGivenCost('D', 'B', 9) == Left("NO SUCH TRACE"))
    }
  }
}
