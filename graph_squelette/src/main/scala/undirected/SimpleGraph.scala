package undirected

import java.security.KeyStore.TrustedCertificateEntry

import scala.annotation.tailrec
import scala.collection.mutable

/** Trait for an undirected and ''simple'' graph, that is without loop nor parallel edges
  * @tparam V type for vertices
  */
trait SimpleGraph[V] {
    /* QUERY METHODS */

    /** The set of all vertices of the graph */
    val vertices : Set[V]

    /** The set of all edges of the graph */
    val edges : Set[Edge[V]]

    /** transform an Option[Set[V]] to a Set[V]
     * @param x Option[Set[V]]
     * @return None if Set[V] is empty or the Set[V] value
     */
    def show(x: Option[Set[V]]): Set[V] = x match {
        case Some(s) => s
        case None => Set()
    }

    /** The set of all vertices adjacent to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the set of all neighbors of `v` otherwise (may be empty)
      */
    def neighborsOf(v : V) : Option[Set[V]]

    /** The number of adjacent vertices to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the degree of `v` otherwise
      */
    def degreeOf(v : V) : Option[Int] = neighborsOf(v) map { _.size }

    /** Checks if there exists a path between two vertices
      * @param v1 one end of path to search
      * @param v2 other end of path to search
      * @return `true` if `v1` and `v2` are equal or if a path exists between `v1` and `v2`, `false` otherwise
      */
    def hasPath(v1 : V, v2 : V) : Boolean = {
        if ((vertices contains v1) && (vertices contains v2))
            hasPath0(v1, v2, Set())
        else
            false
    }

    def hasPath0(v1 : V, v2 : V, verticesObserve : Set[V]) : Boolean = {
        if( !(verticesObserve contains v1) ) {
            if( !( show(neighborsOf(v1)) contains v2 ) || (verticesObserve == Set(v2)) ) {
                //println("v1: "+v1+" neighbors of v1: "+show(neighborsOf(v1))+" v2: "+v2)
                show(neighborsOf(v1)).iterator.foldRight(false)( (v,acc) => {
                    if(acc) true
                    else hasPath0(v, v2, verticesObserve+v1)
                } )
            }
            else {
                true
            }
        } else {
            false
        }
    }

    /** Checks if graph is connected */
    lazy val isConnected : Boolean = vertices.iterator.foldRight(false)((v1,_) => vertices.iterator.foldRight(false)((v2,_) => if(v1==v2) true else hasPath(v1,v2) ))

    /** Checks if graph is acyclic */
    lazy val isAcyclic : Boolean = if (edges.isEmpty) true else !(vertices.iterator.foldRight(false)((v,_) => hasPath(v,v)))

    /** Checks if graph is a tree */
    lazy val isTree : Boolean = isConnected && isAcyclic

    /* VERTEX OPERATIONS */

    /** Add vertex to graph
      * @param v new vertex
      * @return the graph with new vertex `v`
      *         if `v` is an actual vertex of graph, return input graph
      */
    def + (v : V) : SimpleGraph[V]

    /** Remove vertex from graph
      * @param v new vertex
      * @return the graph without vertex `v`
      *         if `v` is not an actual vertex of graph, return input graph
      */
    def - (v : V) : SimpleGraph[V]

    /* EDGE OPERATIONS */

    /** Add edge to graph (also add edge ends as new vertices if necessary)
      * @param e new edge
      * @return the graph with new edge `e`
      *         if `e` is an actual edge of graph, return input graph
      */
    def +| (e : Edge[V]) : SimpleGraph[V]

    /** Remove edge from graph (does NOT remove ends)
      * @param e new edge
      * @return the graph without edge `e`
      *         if `e` is not an actual edge of graph, return input graph
      */
    def -| (e : Edge[V]) : SimpleGraph[V]

    /** Remove all edges from graph but keep same vertices
      * @return graph with same vertices without any edge
      */
    def withoutEdge : SimpleGraph[V]

    /** Add all possible edge with same vertices
      * @return graph with same vertices and all possible edges
      */
    def withAllEdges : SimpleGraph[V]

    /* VALUATED GRAPH METHODS */

    /** Total value of the graph
      * @param valuation valuation used
      * @return total value of the graph, i.e. sum of values of all edges
      */
    def value(valuation : Map[Edge[V], Double]) : Double = (edges map { valuation(_) }).sum

    /** Sorted the valuation of each edges by decreasing value
     * @param valuation valuation used
     * @return LinkedHashMap sorted by decreasing degree
     */
    def sortedValuation(valuation : Map[Edge[V], Double]) : mutable.LinkedHashMap[Edge[V], Double] = mutable.LinkedHashMap(valuation.toList.sortWith(_._2 < _._2):_*)

    /** Minimum spanning tree = Kruskal's Algorithme
      * @param valuation valuation used
      * @return a spanning tree whose value is minimal
      */
    def minimumSpanningTree(valuation : Map[Edge[V], Double]) : SimpleGraph[V] = {
        //print("valuation : "+sortedValuation(valuation))
        //print(sortedValuation(valuation))
        sortedValuation(valuation).foldLeft(SimpleGraphDefaultImpl(Set[V](), Set[Edge[V]]())) { (SimpleGraph, e) =>
          if( (SimpleGraph +| e._1).isAcyclic ) SimpleGraph +| e._1
          else SimpleGraph
        }
    }

    /* COLORING METHODS */

    /** Sequence of vertices sorted by decreasing degree */
    lazy val sortedVertices : Seq[V] = vertices.toSeq.sortWith( (v1,v2) => degreeOf(v1).get > degreeOf(v2).get )

      /** Proper coloring using greedy algorithm (a.k.a WELSH-POWELL) */        

      def coloriageGreedy(sortedV: Seq[V], colored: Map[V, Int]) : Map[V, Int] = {
        if (sortedV.isEmpty) colored
        else coloriageGreedy(sortedV.tail,colored + (sortedV.head -> {
          neighborsOf(sortedV.head).getOrElse(Set()).map(v => {colored.getOrElse(v, 0)}) 
          match {
              case n if n.size == 0 => 1  
              case n => (1 to n.size).find(c => !n.contains(c)).getOrElse(n.size+1)
          }
        }))
      }

      lazy val greedyColoring : Map[V, Int] = {coloriageGreedy(sortedVertices, Map())}
   
     /** Proper coloring using DSATUR algorithm */
    
    /*Calcule de la fonction DSATUR pour une arrete du graphe*/
    def DSAT(v : V, sortedV:List[V], colors:List[Int], sortedVmemoire:List[V],colored:Map[V,Int]) : Int = 
      (sortedV, colors) match{
        case (List(), List()) => 0
        case (List(), color::reste) => DSAT(v,sortedVmemoire,reste,sortedVmemoire,colored)
        case (_, List()) => 0
        case (t::q, color::reste) if colored.contains(t) && neighborsOf(v).contains(t) 
            => if (colored(t)==color) {
              DSAT(v,sortedVmemoire,reste,sortedVmemoire,colored)+1
            } else {DSAT(v,q,color::reste,sortedVmemoire,colored)}
        case (t::q, colors) =>DSAT(v,q,colors,sortedVmemoire,colored)
      }
    /*place l'??l??ment au DSATUR le plus ??leve en premier dans la liste des sommets ?? traiter*/
    def DSATmaxEnTeteDeSortedV(sorted:List[V],colors: List[Int],colored:Map[V,Int]): List[V]={
      if (sorted.isEmpty){return List()}
      val DSATmax=sorted.map{case v=>(v,DSAT(v,sorted,colors,sorted,colored))}.toMap.maxBy{case (key, value) => value }._1
      return(List(DSATmax)++sorted.filter(_!=DSATmax))
    }

    def coloriageDSATUR(sortedV: Seq[V], colored: Map[V, Int]) : Map[V, Int] = {
      if (sortedV.isEmpty) colored
      else coloriageDSATUR(DSATmaxEnTeteDeSortedV(sortedV.tail.toList,colored.values.toList.sortWith(_<_).distinct,colored),colored + (sortedV.head -> {
        neighborsOf(sortedV.head).getOrElse(Set()).map(v => {colored.getOrElse(v, 0)}) 
        match {
            case n if n.size == 0 => 1  
            case n => (1 to n.size).find(c => !n.contains(c)).getOrElse(n.size+1)
        }
      }))
    }
    lazy val coloringDSATUR : Map[V, Int] = {
        
        coloriageDSATUR(sortedVertices, Map())
      }

    //lazy val coloringDSATUR : Map[V, Int] = ColorationDSAT(sortedVertices.toList,List(),Map(),List())
    /* toString-LIKE METHODS */

    /** @inheritdoc */
    override lazy val toString : String = s"({${vertices mkString ", "}}, {${edges mkString ", "}})"

    /** Graph representation in DOT language */
    lazy val toDOTString : String = {
        "strict graph {\n" +
        "    // Edges\n" +
        (edges foldLeft "    ") { _ + _.toDOTString + "\n    " } + "\n" +
        "    // Vertices\n" +
        vertices.mkString("    ", "\n    ", "\n") +
        "  }\n"
      }

}
