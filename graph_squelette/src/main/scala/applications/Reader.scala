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
                52.toInt 
                "azerthyjhtre564rtyrtr".toInt 
            }
        catch { case NonFatal(t) => 0}
        }

    def getAntennes() : Set[Antenne] = {
        
        Source.fromFile("src/main/resources/antennes.csv")
        .getLines()
        .drop(1)
        .map(line => line.split(";"))
        //.filter(_.nonEmpty)
        .filter(_.size == 15)
        //.filter(split => split(11).nonEmpty)
        //.filter(split => tryUse("split(5)") == 0)
        //.filter(split => tryUse(split(7)) == 0)
        //.filter(split => tryUse(split(7)) == true)

        //.filter(_.size = 15) //impose 15, car si 16, on aura des problèmes avec le département
        
        //.filter(LINE DIFFERENT 1 2 3 4 5 6 ... DIFFERENT DE INT)
        //74167;162290002;23;46;0;13;N;0;21;19;E;41;16;16350;16404;
            /*
            id : Int, //1
            COR_NB_DG_LAT : Int, //4
            COR_NB_MN_LAT : Int, //5
            COR_NB_SC_LAT : Int, //6
            COR_CD_NS_LAT : Int, //7

            COR_NB_DG_LON : Int, //8
            COR_NB_MN_LON : Int, //9
            COR_NB_SC_LON : Int, //10
            COR_CD_EW_LON : Int, //11
            departement : String //14
            */

        /* val strings = list.filter {
     |     case s: String => true
     |     case _ => false
     |      }*/
        .map(split => Antenne(
        split(1) ,
         split(4).toInt , 
          split(5).toInt , split(6)(0) ,split(7).toInt ,
         split(8).toInt , split(9).toInt , split(10)(0) ,split(11).toInt,
         split(14)))
        .toSet
    }
}
