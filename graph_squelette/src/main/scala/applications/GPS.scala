package application

import scala.annotation.tailrec
import directed.Arc
import directed.StrictGraphMatrixImpl
import scala.io.StdIn.readLine

case class InterestPoint(name : String, lat : Double, lon: Double) {
  /** @inheritdoc */
  override lazy val toString : String = name
}

object GPS {
  
  val AVERAGE_RADIUS_OF_EARTH_KM = 6371

  lazy val interestPoints : Seq[InterestPoint] = Seq (
    InterestPoint("Tour Eiffel",48.85942046165691, 2.2943181963424464),
    InterestPoint("Arc de Triomphe",48.87579019009461, 2.2946031141516925),
    InterestPoint("Palais Garnier",48.873855403612204, 2.3311644658482904),
    InterestPoint("Musee du Louvre",48.860478736714654, 2.3356513296155166),
    InterestPoint("Place de la Concorde",48.86664333254297, 2.3213697005897047),
    InterestPoint("Notre-Dame de Paris",48.85348636888494, 2.3497622935780194)
  )
  
  lazy val adjacency : IndexedSeq[IndexedSeq[Int]] = IndexedSeq (
    IndexedSeq(0,0,1,1,1,1),
    IndexedSeq(1,0,1,1,1,1),
    IndexedSeq(1,1,0,1,1,1),
    IndexedSeq(1,1,1,0,1,1),
    IndexedSeq(1,1,1,1,0,1),
    IndexedSeq(1,1,1,1,1,0)
  )
  
  // Adjacency matrix containing the speed limit in km/h between 2 InterestPoints
  lazy val speedLimits : IndexedSeq[IndexedSeq[Double]] = IndexedSeq(
    IndexedSeq(0.0, 6.5, 6.5, 6.5, 6.5, 6.5),
    IndexedSeq(6.5, 0.0, 6.5, 6.5, 6.5, 6.5),
    IndexedSeq(6.5, 6.5, 0.0, 6.5, 6.5, 6.5),
    IndexedSeq(6.5, 6.5, 6.5, 0.0, 6.5, 6.5),
    IndexedSeq(6.5, 6.5, 6.5, 6.5, 0.0, 6.5),
    IndexedSeq(6.5, 6.5, 6.5, 6.5, 6.5, 0.0)
  )

  lazy val city : StrictGraphMatrixImpl[InterestPoint] = StrictGraphMatrixImpl(interestPoints, adjacency)

  /** @inheritdoc */
  override lazy val toString : String = s"Interest Points:\n ${interestPoints mkString "\n"} \n\n Adjacency:\n ${adjacency mkString "\n"} \n\n SpeedLimit:\n ${speedLimits mkString "\n"}"

  def main(args: Array[String]) = {
    displayInstructions()

    displayAvailablePoints(interestPoints)
    val startPoint: InterestPoint = askStartPoint(interestPoints)
    val availablePoints = interestPoints.filterNot(elm => elm == startPoint)

    displayAvailablePoints(availablePoints)
    val endPoint: InterestPoint = askEndPoint(availablePoints)
    
    val valuation = city.arcs.map(a => (a, arcToDistance(a) / speedLimits(interestPoints.indexOf(a._1))(interestPoints.indexOf(a._2)))).toMap
    val path: Option[(Seq[InterestPoint], Double)] = city.shortestPath(valuation)(startPoint, endPoint)
    displayPath(path, startPoint, endPoint)
  }

  def displayAvailablePoints(availablePoints : Seq[InterestPoint]): Unit = {
    println("Available Interest points are: \n")
    (0 to availablePoints.size-1).map{i => println(i.toString + ") " + availablePoints(i).name)}
    println()
  }

  def displayInstructions(): Unit = {
    println("Welcome to the city!\n\n" + this + "\n")
  }

  def askStartPoint(availablePoints: Seq[InterestPoint]): InterestPoint = {
    println("Pick a starting point: \n")
    val n = scala.io.StdIn.readInt()
    availablePoints(n)
  }

  def askEndPoint(availablePoints: Seq[InterestPoint]): InterestPoint = {
    println("Pick an end point: \n")
    val n = scala.io.StdIn.readInt()
    availablePoints(n)
  }

  def displayPath(path: Option[(Seq[InterestPoint], Double)], start : InterestPoint, end: InterestPoint): Unit = {
    path match{
      case Some(x) => {
                        println("\nShortest path from " + start.name + " to " + end.name + " is :\n")
                        x._1.map{place => println(place.name +"\n")}
                        val distance = x._1.foldLeft((start,0.0)){(acc,y)=> (y, acc._2 + arcToDistance(Arc(acc._1,y))) }
                        println("This path have a length of " + distance._2 + " km and will take " + (x._2 * 60) + " minutes.\n")
                      }
      case None => println("There is no path from " + start.name + " to " + end.name + "\n")
    }
  }

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

  def arcToDistance(arc :Arc[InterestPoint]) = {
    calculateDistanceInKilometer(arc._1.lat, arc._1.lon, arc._2.lat, arc._2.lon)
  }
}
