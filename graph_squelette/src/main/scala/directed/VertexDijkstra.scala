package directed

/** Vertex with more attributes used for the shortestPath() in StrictGraph.scala
  * @param v vertex
  * @param visited boolean indicating if the vertex has been visited
  * @param distance distance from the start and vertex v
  * @param arc [[None]] if there is path to v, arc leading to v on the shortest path otherwise
  * @tparam V type of vertex
  */
case class VertexDijkstra[V](v : V, visited : Boolean, distance : Double, arc : Option[Arc[V]]) {
    /** VertexDijkstra representation */
    lazy val toDOTString : String = s"[${v},${visited},${distance},${arc}]"

    /** @inheritdoc */
    override lazy val toString : String = toDOTString
  }