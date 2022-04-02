package undirected

import org.scalacheck.Gen
import org.scalacheck.Gen._

/** The test class for [[SimpleGraphMatrixImplSpec]] implementation */
class SimpleGraphMatrixImplSpec extends SimpleGraphSpec(SimpleGraphMatrixImplSpec)

object SimpleGraphMatrixImplSpec extends SimpleGraphSpecCompanion[Int]("SimpleGraphMatrixImpl") {

  /** @inheritdoc */
  val vertex : Gen[Int] = posNum[Int]

  def matrixFromVerticesAndEdges[V](vertices: Seq[V], edges: Set[Edge[V]]) : IndexedSeq[IndexedSeq[Int]] =
    vertices.map(x => IndexedSeq(vertices.map(y => if(edges.contains(Edge(x,y))) 1 else 0 )).flatten).toIndexedSeq

  /** @inheritdoc */
    def graphWithAtLeast(vertexMinCount: Int, edgeMinCount: Int = 0): Gen[SimpleGraphMatrixImpl[Int]] =
      for(
        vertexAdditionalCount <- posNum[Int] ;
        vertexCount <- Gen.const((vertexMinCount + vertexAdditionalCount) max 1) ;
        vs <- Gen.containerOfN[Set, Int](vertexCount, vertex) ;
        edgeCount <- Gen.choose(edgeMinCount, vertexCount * (vertexCount - 1) / 2) ;
        es <- Gen.containerOfN[Set, Edge[Int]](edgeCount, edgeFrom(vs))
      ) yield SimpleGraphMatrixImpl(vs.toSeq, matrixFromVerticesAndEdges[Int](vs.toSeq, es))

}