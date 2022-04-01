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
        vs.zipWithIndex.map(eVs => adjacency(eVs._2).zipWithIndex.filter(p => p._1 == 1).map(eIsArc => Arc(eVs._1, vs(eIsArc._2)) ) ).flatten.toSet
    }

    /** @inheritdoc */
    def successorsOf(v : V) : Option[Set[V]] = {  
        val index = vs.indexOf(v)
        if (!(vs contains v)) None
        else Some(adjacency(index).zipWithIndex.filter(pair => pair._1 == 1).map(pair => vs(pair._2)).toSet)
    }

    /** @inheritdoc */
    def + (v : V) : StrictGraphMatrixImpl[V] = {
        if (vs contains v) StrictGraphMatrixImpl(vs, adjacency)
        else StrictGraphMatrixImpl(vs :+ v, adjacency.map(x=> x :+ 0) :+ IndexedSeq.fill(vs.size+1)(0))
    }

    /** @inheritdoc */
    def - (v : V) : StrictGraphMatrixImpl[V] = {
      val index = vs.indexOf(v)
      if (!vs.contains(v)) StrictGraphMatrixImpl(vs, adjacency)
      else
      {
        val newAdjacency = adjacency.patch(index, Nil, 1).map(l => l.patch(index, Nil, 1))
        StrictGraphMatrixImpl(vs.filter(_ != v), newAdjacency)
      }
    }

    /** @inheritdoc */
    def +| (e : Arc[V]) : StrictGraphMatrixImpl[V] = {
        //if (!(vs contains e._1)) this + e._1
        //if (!(vs contains e._2)) this + e._2
        
        val iSource = vs.indexOf(e._1)
        val iDest = vs.indexOf(e._2)
        val newAdjacency = adjacency.updated(iSource, adjacency(iSource).updated(iDest, 1))
        StrictGraphMatrixImpl(vs, newAdjacency)

    }

    /** @inheritdoc */
    def -| (e : Arc[V]) : StrictGraphMatrixImpl[V] = {
        if (!vs.contains(e._1) || !vs.contains(e._2)) StrictGraphMatrixImpl(vs, adjacency)
        else {
            val iSource = vs.indexOf(e._1)
            val iDest = vs.indexOf(e._2)
            val newAdjacency = adjacency.updated(iSource, adjacency(iSource).updated(iDest, 0))
            StrictGraphMatrixImpl(vs, newAdjacency)
        }
    }

    /** @inheritdoc */
    def withoutArc : StrictGraphMatrixImpl[V] = StrictGraphMatrixImpl(vs, IndexedSeq.fill(vs.size)(IndexedSeq.fill(vs.size)(0)))

    /** @inheritdoc */
    def withAllArcs : StrictGraphMatrixImpl[V] = ???
}

