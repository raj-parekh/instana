package util

import models.ConnectedNode
import org.scalatest.FunSpec

class FileHelperTest extends FunSpec {

  describe("test reading file and creating dictionary") {
    it("should validate size of each adjacency list provided after processing the file") {
      new FileHelper {
        private val adjMap: Map[Int, List[ConnectedNode]] = readFileAndUpdateDictionary("src/test/resources/test.csv")

        assert(adjMap(0).size == 3)
        assert(adjMap(1).size == 1)
        assert(adjMap(2).size == 2)
        assert(adjMap(3).size == 2)
        assert(adjMap(4).size == 1)
      }
    }
  }
}
