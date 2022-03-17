package undirected

/** Implementation of [[SimpleGraph]] using list of neighbors for each vertex
  * @param neighbors associative map providing set of neighbors for each vertex
  *                  Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class SimpleGraphNeighborsImpl[V](neighbors : Map[V, Set[V]]) extends SimpleGraph[V] {

    /** @inheritdoc */
    val vertices : Set[V] = neighbors.keySet

    /** @inheritdoc */
    val edges : Set[Edge[V]] = (neighbors map {case (k, v) => (k, v.map(c => Edge(k, c)))}).values.flatten.toSet

    /** @inheritdoc */
    def neighborsOf(v: V) : Option[Set[V]] = neighbors get v

    /** @inheritdoc */
    def + (v : V) : SimpleGraphNeighborsImpl[V] = if (neighbors contains v) SimpleGraphNeighborsImpl(neighbors) else SimpleGraphNeighborsImpl(neighbors + (v -> Set())) 

    /** @inheritdoc */
    def - (v : V) : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl((neighbors - v) map {case (k, a) => (k, a filterNot {e=> e == v})})

    /** @inheritdoc */
    def +| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] =
      SimpleGraphNeighborsImpl(
        neighbors + 
        (e._1 -> ((neighbors getOrElse (e._1, Set())) + e._2)) +
        (e._2 -> ((neighbors getOrElse (e._2, Set())) + e._1))
      )

    /** @inheritdoc */
    def -| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] =
      if ((neighbors contains e._1) && (neighbors contains e._2))
        SimpleGraphNeighborsImpl(
          neighbors +
          (e._1 -> (neighbors(e._1) - e._2)) +
          (e._2 -> (neighbors(e._2) - e._1))
        )
      else
        SimpleGraphNeighborsImpl(neighbors)

    /** @inheritdoc */
    def withoutEdge : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl((neighbors map {case (k, v) => (k, (v.empty))}))

    /** @inheritdoc */
    def withAllEdges : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl((neighbors map {case (k, v) => (k, (v ++ neighbors.keySet - k))}))
}
