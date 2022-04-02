package undirected

/** Implementation of [[SimpleGraph]] using adjacency matrix
  * @param vs sequence of vertices in the order they are used in adjacency matrix
  * @param adjacency adjacency matrix
  * @tparam V type for vertices
  */
case class SimpleGraphMatrixImpl[V](vs : Seq[V], adjacency : IndexedSeq[IndexedSeq[Int]]) extends SimpleGraph[V] {

    /** @inheritdoc */
    lazy val vertices : Set[V] = vs.toSet[V];

    /** @inheritdoc */
    lazy val edges : Set[Edge[V]] = {
        vs.zipWithIndex.map(eVs => adjacency(eVs._2).zipWithIndex.filter(p => p._1 == 1).map(eIsArc => Edge(eVs._1, vs(eIsArc._2)) ) ).flatten.toSet
    }

    /** @inheritdoc */
    def neighborsOf(v : V) : Option[Set[V]] =
        if (!(this.vertices contains v)) None else Some(this.vertices filter { this.edges contains Edge(v, _) })

    /** @inheritdoc */
    def + (v : V) : SimpleGraphMatrixImpl[V] =
        if (vs contains v) SimpleGraphMatrixImpl(vs, adjacency) else SimpleGraphMatrixImpl(vs :+ v, adjacency :+ IndexedSeq.fill(adjacency.size)(0))

    /** @inheritdoc */
    def - (v : V) : SimpleGraphMatrixImpl[V] = {
        val index = vs.indexOf(v)
        if (!vs.contains(v)) SimpleGraphMatrixImpl(vs, adjacency)
        else
        {
            val newAdjacency = adjacency.patch(index, Nil, 1).map(l => l.patch(index, Nil, 1))
            SimpleGraphMatrixImpl(vs.filter(_ != v), newAdjacency)
        }
    }

    /** @inheritdoc */
    def +| (e : Edge[V]) : SimpleGraphMatrixImpl[V] = {
        // Create a new adjacency matrix with same edges then before and potential new vertices
        val newVs = (vs :+ e._1 :+ e._2).distinct
        val tmpAdjacency = (0 to newVs.size-1).map(x => IndexedSeq((0 to newVs.size-1).map(y => if (x < vs.size && y < vs.size) adjacency(x)(y) else 0 )).flatten).toIndexedSeq

        // Add edge to adjacency matrix
        val iSource = newVs.indexOf(e._1)
        val iDest = newVs.indexOf(e._2)
        val e1Adjacency = tmpAdjacency.updated(iSource, tmpAdjacency(iSource).updated(iDest, 1))
        val newAdjacency = e1Adjacency.updated(iDest, e1Adjacency(iDest).updated(iSource, 1))
        SimpleGraphMatrixImpl(newVs, newAdjacency)
    }

    /** @inheritdoc */
    def -| (e : Edge[V]) : SimpleGraphMatrixImpl[V] = 
        if ( !(this.edges contains e) ) SimpleGraphMatrixImpl(vs, adjacency)
        else SimpleGraphMatrixImpl(vs, vs.map{ case v =>
                                                        if(v == e._1) { adjacency(vs.indexOf(v)).updated(vs.indexOf(e._2), 0) }
                                                        else if (v == e._2) { adjacency(vs.indexOf(v)).updated(vs.indexOf(e._1), 0) }
                                                        else adjacency(vs.indexOf(v)) }.toIndexedSeq )

    /** @inheritdoc */
    def withoutEdge : SimpleGraphMatrixImpl[V] = SimpleGraphMatrixImpl( vs, IndexedSeq.fill(vs.size)(IndexedSeq.fill(vs.size)(0)) )

    /** @inheritdoc */
    def withAllEdges : SimpleGraphMatrixImpl[V] = SimpleGraphMatrixImpl( vs, vs.map(v => IndexedSeq.fill(vs.size)(IndexedSeq.fill(vs.size)(1))(vs.indexOf(v)).updated( vs.indexOf(v), 0 )).toIndexedSeq )
    //2 stages to update adjacency
    //val withAllEdges = IndexedSeq.fill(vs.size)(IndexedSeq.fill(vs.size)(1))
    //vs.map(v => withAllEdges(vs.indexOf(v)).updated( vs.indexOf(v), 0 ))
  }

