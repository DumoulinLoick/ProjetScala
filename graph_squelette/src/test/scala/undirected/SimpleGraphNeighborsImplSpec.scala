package undirected

import org.scalacheck.Gen
import org.scalacheck.Gen._

/** The test class for [[SimpleGraphNeighborsImpl]] implementation */
class SimpleGraphNeighborsImplSpec extends SimpleGraphSpec(SimpleGraphNeighborsImplSpec)

object SimpleGraphNeighborsImplSpec extends SimpleGraphSpecCompanion[Int]("SimpleGraphNeighborsImpl") {
    /** @inheritdoc */
    val vertex : Gen[Int] = posNum[Int]

    def neighborsFromVerticesAndEdges[V](vertices: Set[V], edges: Set[Edge[V]]): Map[V, Set[V]] = 
      vertices.foldLeft(Map.empty[V, Set[V]]){(acc, x) => acc + (x -> edges.filter((ed:Edge[V]) => ed._1 == x || ed._2 == x).map(e => if (x == e._1)  e._2 else e._1))}

    /** @inheritdoc */
    def graphWithAtLeast(vertexMinCount: Int, edgeMinCount: Int = 0): Gen[SimpleGraphNeighborsImpl[Int]] =
      for(
        vertexAdditionalCount <- posNum[Int] ;
        vertexCount <- Gen.const((vertexMinCount + vertexAdditionalCount) max 1) ;
        vs <- Gen.containerOfN[Set, Int](vertexCount, vertex) ;
        edgeCount <- Gen.choose(edgeMinCount, vertexCount * (vertexCount - 1) / 2) ;
        es <- Gen.containerOfN[Set, Edge[Int]](edgeCount, edgeFrom(vs))
      ) yield SimpleGraphNeighborsImpl(neighborsFromVerticesAndEdges[Int](vs, es))
  }
