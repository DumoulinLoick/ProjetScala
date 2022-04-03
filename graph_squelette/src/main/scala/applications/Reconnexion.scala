package applications

import undirected.{Edge, SimpleGraphDefaultImpl,SimpleGraphMatrixImpl,SimpleGraphNeighborsImpl}
import scala.collection.immutable
import scala.io.Source



object Reconnexion extends App {

    def transform(antennes : Set[Antennes]) : Map[Edge[V], Double]  = {
        


    }


    val antennes = Reader.getAntennes().filter(_.departement == "16166")
    println(s"${antennes.size} antennes")  

    val graph = SimpleGraphDefaultImpl(antennes, Set[Edge[Antenne]]())
    //val applicationTREE = graph.minimumSpanningTree(valuation)
    val test = antennes.toList
    test.foreach(println)   
     test.foreach(println)   
    //test.split(" ").foreach(println)(",")//.foreach(println)
    println(s"aaaa ${test(1)} aaaaa")
    //tests perso
    /*val test = antennes.toList
    test.foreach(println)
    val a = test.
    antennes.distance(10.2,11.5)*/




    //val coutReconnexion = graph.edges.map(edge => (edge, edge.length)).toMap
    //println(s"${coutReconnexion.size} arcs")
    //caluation : une map associe un arc a une longueur

    // valuation Map[Edge[V], Double]

    //48.97067508254935, 2.3286418451428674

    //println(antennes.distance(48.97067508254935,2.3286418451428674))
    //val a = antennes.Distance(48.97067508254935,2.3286418451428674)
    //
}
