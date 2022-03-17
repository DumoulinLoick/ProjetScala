package directed

/** Implementation of [[StrictGraph]] using list of successors for each vertex
  * @param successors associative map providing set of successors for each vertex
  *                   Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class StrictGraphSuccessorsImpl[V](successors : Map[V, Set[V]]) extends StrictGraph[V] {

    /** @inheritdoc */
    val vertices : Set[V] = successors.keySet

    /** @inheritdoc */
    val arcs : Set[Arc[V]] = (successors map {case (k, v) => (k, v.map(c => Arc(k, c)))}).values.flatten.toSet

    /** @inheritdoc */
    def successorsOf(v: V) : Option[Set[V]] = successors get v

    /** @inheritdoc */
    def + (v : V) : StrictGraphSuccessorsImpl[V] = if (successors contains v) StrictGraphSuccessorsImpl(successors) else StrictGraphSuccessorsImpl(successors + (v -> Set()))

    /** @inheritdoc */
    def - (v : V) : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl((successors - v) map {case (k, a) => (k, a filterNot {e=> e == v})})

    /** @inheritdoc */
    def +| (e: Arc[V]) : StrictGraphSuccessorsImpl[V] =
      StrictGraphSuccessorsImpl(
        successors + 
        (e._1 -> ((successors getOrElse (e._1, Set())) + e._2)) +
        (e._2 -> ((successors getOrElse (e._2, Set()))))
      )

    /** @inheritdoc */
    def -| (e: Arc[V]) : StrictGraphSuccessorsImpl[V] = 
      if ((successors contains e._1) && (successors contains e._2))
          StrictGraphSuccessorsImpl(
            successors +
            (e._1 -> (successors(e._1) - e._2))
          )
        else
          StrictGraphSuccessorsImpl(successors)

    /** @inheritdoc */
    def withoutArc : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl((successors map {case (k, v) => (k, (v.empty))}))

    /** @inheritdoc */
    def withAllArcs : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl((successors map {case (k, v) => (k, (v ++ successors.keySet - k))}))
  }

