package applications

import undirected.{Edge, SimpleGraphDefaultImpl,SimpleGraphMatrixImpl,SimpleGraphNeighborsImpl}
import scala.collection.immutable.ListMap

object Reconnexion extends App {

    val antennes = Reader.getAntennes()
    println(s"${antennes.size} antennes")
   /* for (i <- 0 until antennes.size) {
        println(s"${antennes[i].id} : ${antennes[i].longitude} ${antennes[i].latitude}")
    }*/

    val a = antennes
}
