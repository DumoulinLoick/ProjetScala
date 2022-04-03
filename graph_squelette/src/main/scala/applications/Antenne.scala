package applications

import scala.math.{acos, sin, cos, sqrt, pow, toRadians}

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

     def distance( latitude1 : Double, longitude1 : Double) : Double = {
       /* acos( 
            sin(toRadians(latitude1)) * sin(toRadians(latitude))
            + cos(toRadians(latitude1)) * cos(toRadians(latitude)) * cos(toRadians(longitude1-longitude))
        ) * 6371*/
        852
    }

}
