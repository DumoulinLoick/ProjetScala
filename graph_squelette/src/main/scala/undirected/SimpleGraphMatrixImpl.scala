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
        if (vs contains v) SimpleGraphMatrixImpl(vs, adjacency) else SimpleGraphMatrixImpl(vs :+ v, adjacency :+ IndexedSeq.fill(adjacency.size)(0)) //add +1 in adjacency.size ???

    /** @inheritdoc */
    def - (v : V) : SimpleGraphMatrixImpl[V] =
        if (!(vs contains v)) SimpleGraphMatrixImpl(vs, adjacency)
        else SimpleGraphMatrixImpl( vs.diff(Seq(v)), adjacency.diff(IndexedSeq(adjacency(vs indexOf(v)))).collect{case e => e.diff(IndexedSeq( e( vs indexOf(v) ) ))} )//result.collect{case e => e.diff(IndexedSeq( e(2) ))}
        //val original = IndexedSeq("a", "b", "a", "c", "d", "a")
        //val exclude = IndexedSeq("a", "d", "a")
        //val result = original.diff(exclude)

    /** @inheritdoc */
    def +| (e : Edge[V]) : SimpleGraphMatrixImpl[V] =
        if ( !(vs contains e._1) || !(vs contains e._2) ) SimpleGraphMatrixImpl(vs, adjacency)
        else SimpleGraphMatrixImpl(vs, vs.map{ case v =>
                                                        if(v == e._1) { adjacency(vs.indexOf(v)).updated(vs.indexOf(e._2), 1) }
                                                        else if (v == e._2) { adjacency(vs.indexOf(v)).updated(vs.indexOf(e._1), 1) }
                                                        else adjacency(vs.indexOf(v)) }.toIndexedSeq //verify !!!
        )

    /** @inheritdoc */
    def -| (e : Edge[V]) : SimpleGraphMatrixImpl[V] = //same technique as +|
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

