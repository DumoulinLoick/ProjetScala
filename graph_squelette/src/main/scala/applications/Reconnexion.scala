package applications

import undirected.{Edge, SimpleGraphDefaultImpl,SimpleGraphMatrixImpl,SimpleGraphNeighborsImpl}
import scala.collection.immutable.ListMap

object Reconnexion extends App {

    val antennes = Reader.getAntennes().filter(_.departement == "16166")
    println(s"${antennes.size} antennes")


    val graph = SimpleGraphDefaultImpl(antennes, Set[Edge[Antenne]]())
    
    val coutReconnexion = graph.edges.map(edge => (edge, edge.length)).toMap
    println(s"${coutReconnexion.size} arcs")
    //caluation : une map associe un arc a une longueur
    // valuation Map[Edge[V], Double]

    //val applicationTREE = graph.minimumSpanningTree(valuation)
}
