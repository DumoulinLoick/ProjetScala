package application

import applications.{Reader,Antenne}
import scala.annotation.tailrec
import undirected.SimpleGraphNeighborsImpl

object affectation extends App{
    val antennes = Reader.getAntennes().filter(_.departement == "16166")
    


    val AVERAGE_RADIUS_OF_EARTH_KM = 6371

    def calculateDistanceInKilometer(startLat: Double, startLon : Double, endLat: Double, endLon : Double): Double = {
      val latDistance = Math.toRadians(startLat - endLat)
      val lngDistance = Math.toRadians(startLon - endLon)
      val sinLat = Math.sin(latDistance / 2)
      val sinLng = Math.sin(lngDistance / 2)
      val a = sinLat * sinLat +
      (Math.cos(Math.toRadians(startLat)) *
          Math.cos(Math.toRadians(endLat)) *
          sinLng * sinLng)
      val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
      (AVERAGE_RADIUS_OF_EARTH_KM * c).toDouble
    }
/*
    def AjouterVoisin(antenne1:Antenne,antenne2:Antenne,matrice:Map[Int,Set[Int]]):Map[Int,Set[Int]]={
        if (matrice.keys.toList.contains(antenne1.id.toInt)){
            return (matrice(antenne1.id.toInt)+(antenne2.id.toInt))
        }
        return(matrice+(antenne1.id.toInt->Set(antenne2.id.toInt)))
    }
*/

    def AntenneToGraphe (distanceLim:Double,antennes1:List[Antenne],antennes2:List[Antenne],antennesSave:List[Antenne]):Map[Int,Set[Int]]={
        (antennes1,antennes2) match{
            case (List(),_)=>Map()
            case (t::q,List()) =>AntenneToGraphe(distanceLim,q,antennesSave,antennesSave)
            case (t1::q1,t2::q2) if (calculateDistanceInKilometer(t1.latitude,t1.longitude,t2.latitude,t2.longitude)<distanceLim) => AntenneToGraphe(distanceLim,t1::q1,q2,antennesSave)
            case (t1::q1,t2::q2) => AntenneToGraphe(distanceLim,t1::q1,q2,antennesSave)
        }
    }

    //println(calculateDistanceInKilometer(antennes.head.latitude,antennes.head.longitude,antennes.tail.head.latitude,antennes.tail.head.longitude))

    //println(yuki.minimumSpanningTree())  }
}

