package applications

import scala.io.Source
import scala.util.control.NonFatal

object Reader {

    def tryUse(car : String) : Integer = {

             /* try {
                    "car".toInt
                } catch { case _: Throwable => 0 }
            */
                try {
                car.toInt 
            }
        catch { 
            case NonFatal(t) => 1
        }
    }

    def getAntennes() : Set[Antenne] = {
        
        Source.fromFile("src/main/resources/antennes.csv")
        .getLines()
        .drop(1)
        .map(line => line.split(";"))
        .filter(_.size == 15)
        .filter(split => tryUse(split(4)) != 1)
        .filter(split => tryUse(split(5)) != 1)
        .filter(split => tryUse(split(7)) != 1)
        .filter(split => tryUse(split(8)) != 1)
        .filter(split => tryUse(split(9)) != 1)
        .filter(split => tryUse(split(11))!= 1)
        .map(split => Antenne(
            split(1) ,
            split(4).toInt , 
            split(5).toInt , split(6) , split(7).toInt ,
            split(8).toInt , split(9).toInt , split(10) , split(11).toInt ,
            split(14) ))
        .toSet
    }
}
