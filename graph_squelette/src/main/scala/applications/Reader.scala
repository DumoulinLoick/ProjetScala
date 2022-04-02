package applications

import scala.io.Source

object Reader {
    def getAntennes() : Set[Antenne] = {
        Source.fromFile("src/main/resources/antennes.csv")
        .getLines()
        .drop(1)
        .map(line => line.split(";"))
        .map(line => Antenne(line(1)))
        .toSet
    }
}