package applications

import scala.io.StdIn.readLine


final case class Antenne
( 
  id : String, //1
  COR_NB_DG_LAT : Int, //4
  COR_NB_MN_LAT : Int, //5
  COR_NB_SC_LAT : String, //6
  COR_CD_NS_LAT : Int, //7

  COR_NB_DG_LON : Int, //8
  COR_NB_MN_LON : Int, //9
  COR_NB_SC_LON : String, //10
  COR_CD_EW_LON : Int, //11
  departement : String //14
)
{  
  //cacul de la latitude et longitude
    val latitude = (COR_NB_DG_LAT + COR_NB_MN_LAT/60.0 + COR_CD_NS_LAT/3600.0) * (if ( COR_NB_SC_LAT== "S") 1 else -1)
    val longitude = (COR_NB_DG_LON + COR_NB_MN_LON/60.0 + COR_CD_EW_LON/3600.0) * (if (COR_NB_SC_LON == "W") 1 else -1)

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

}
