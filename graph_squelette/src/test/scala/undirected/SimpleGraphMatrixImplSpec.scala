package undirected

import org.scalacheck.Gen
import org.scalacheck.Gen._


/** The test class for [[SimpleGraphMatrixImplSpec]] implementation */
class SimpleGraphMatrixImplSpec extends SimpleGraphSpec(SimpleGraphMatrixImplSpec)

object SimpleGraphMatrixImplSpec extends SimpleGraphSpecCompanion[Int]("SimpleGraphMatrixImpl") {

    /** @inheritdoc */
    val vertex : Gen[Int] = posNum[Int]

 def matrixFromVerticesAndEdges[V](vertices: Set[V], edges: List[(Int,Int)]) : IndexedSeq[IndexedSeq[Int]] =
      edges match{
        case List()=>IndexedSeq.fill(vertices.size)(IndexedSeq.fill(vertices.size)(0))
        case t::q=>matrixFromVerticesAndEdges(vertices,q).updated(t(0),matrixFromVerticesAndEdges(vertices,q)(1).updated(t(1),1)).updated(t(1),matrixFromVerticesAndEdges(vertices,q)(1).updated(t(0),1))
      }

  /** @inheritdoc */
    def graphWithAtLeast(vertexMinCount: Int, edgeMinCount: Int = 0): Gen[SimpleGraphMatrixImpl[Int]] =
      for(
        vertexAdditionalCount <- posNum[Int] ;
        vertexCount <- Gen.const((vertexMinCount + vertexAdditionalCount) max 1) ;
        vs <- Gen.containerOfN[Set, Int](vertexCount, vertex) ;
        edgeCount <- Gen.choose(edgeMinCount, vertexCount * (vertexCount - 1) / 2) ;
        es <- Gen.containerOfN[Set, Edge[Int]](edgeCount, edgeFrom(vs))
      ) yield SimpleGraphMatrixImpl(vs.toSeq, matrixFromVerticesAndEdges[Int](vs, ed))
}