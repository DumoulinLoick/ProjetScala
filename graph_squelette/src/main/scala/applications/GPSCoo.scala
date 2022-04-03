import scala.math.{acos, sin, cos, sqrt, pow, toRadians}

final case class GPSCoo(COR_NB_DG_LAT: Int, COR_NB_MN_LAT: Int, COR_NB_SC_LAT : Char,COR_CD_NS_LAT : Int,COR_NB_DG_LON : Int, COR_NB_MN_LON : Int, COR_NB_SC_LON : Char,COR_CD_EW_LON : Int,departement : String) {
    lazy val latitude : Double = {
        (COR_NB_DG_LAT + COR_NB_MN_LAT/60.0 + COR_NB_SC_LAT/3600.0) * (if (COR_NB_SC_LAT == 'S') -1 else 1)
    }

    lazy val longitude : Double = {
        (COR_CD_NS_LAT + COR_NB_DG_LON/60.0 + COR_NB_MN_LON/3600.0) * (if (COR_NB_SC_LON == 'W') -1 else 1)
    }

    // calculate the flat distance to another GPS coordinate (not using elevation)
    def Distance(o: GPSCoo) : Double = {
        acos(
            sin(toRadians(o.latitude)) * sin(toRadians(latitude))
            + cos(toRadians(o.latitude)) * cos(toRadians(latitude)) * cos(toRadians(o.longitude-longitude))
        ) * 6371
    }

    def - (o : GPSCoo) : Double = {
        // use Pythagore to find the distance between 2 points using flat distance and elevation (in meters)
        sqrt(pow(Distance(o), 2) + pow(COR_CD_EW_LON/1000.0-o.COR_CD_EW_LON/1000.0, 2))
    }

    /** @inheritdoc */
    override lazy val toString : String = {
        s"${latitude}\"${longitude}"
    }
}