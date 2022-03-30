package directed

/** Implementation of [[StrictGraph]] using adjacency matrix
  * @param vs sequence of vertices in the order they are used in adjacency matrix
  * @param adjacency adjacency matrix
  * @tparam V type for vertices
  */
case class StrictGraphMatrixImpl[V](vs : Seq[V], adjacency : IndexedSeq[IndexedSeq[Int]]) extends StrictGraph[V] {

    /** @inheritdoc */
    lazy val vertices : Set[V] = {
        vs.toSet
    }

    /** @inheritdoc */
    lazy val arcs : Set[Arc[V]] = {
      //IDK how to do this 
    }

    /** @inheritdoc */
    def successorsOf(v : V) : Option[Set[V]] = {  
        val index = vs.indexOf(v)
        if (index < 0) None
        //else Some(adjacency(index).filter(_ > 0).map(vs(_)).toSet)
        else Some(adjacency(index).filter(_ > 0).map(vs).toSet)
    }

    /** @inheritdoc */
    def + (v : V) : StrictGraphMatrixImpl[V] = {
        val newVs = vs :+ v
        val newAdjacency = adjacency :+ IndexedSeq.fill(newVs.size)(0)
        StrictGraphMatrixImpl(newVs, newAdjacency)
    }

    /** @inheritdoc */
    def - (v : V) : StrictGraphMatrixImpl[V] = {
        val index = vs.indexOf(v)
        if (index < 0) this
        else {
            val newAdjacency = adjacency.map(_.map(_ - 1))
            StrictGraphMatrixImpl(vs.filter(_ != v), newAdjacency)
        }
    }

    /** @inheritdoc */
    def +| (e : Arc[V]) : StrictGraphMatrixImpl[V] = {
        val (v1, v2) = e
        val index1 = vs.indexOf(v1)
        val index2 = vs.indexOf(v2)
        //if first or second node is not in the graph, return the same graph
        if (index1 < 0 || index2 < 0) this
        else {
          //AAAAAAAAAAAAAAAAAAAAAAAAAAA
        }
    }

    /** @inheritdoc */
    def -| (e : Arc[V]) : StrictGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def withoutArc : StrictGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def withAllArcs : StrictGraphMatrixImpl[V] = ???
}

