package directed

import org.scalacheck.Gen
import org.scalacheck.Gen._

/** The test class for [[StrictGraphMatrixImpl]] implementation */
class StrictGraphMatrixImplSpec extends StrictGraphSpec(StrictGraphMatrixImplSpec)

object StrictGraphMatrixImplSpec extends StrictGraphSpecCompanion[Int]("StrictGraphMatrixImpl") {
    /** @inheritdoc */
    val vertex : Gen[Int] = posNum[Int]

    def matrixFromVerticesAndArcs[V](vertices: Seq[V], arcs: Set[Arc[V]]) : IndexedSeq[IndexedSeq[Int]] =
        vertices.map(x => IndexedSeq(vertices.map(y => if(arcs.contains(Arc(x,y))) 1 else 0 )).flatten).toIndexedSeq

    /** @inheritdoc */
    def graphWithAtLeast(vertexMinCount: Int, arcMinCount: Int = 0): Gen[StrictGraphMatrixImpl[Int]] =
      for(vertexAdditionalCount <- posNum[Int]; vertexCount <- Gen.const((vertexMinCount + vertexAdditionalCount) max 1);
          vs <- Gen.containerOfN[Set, Int](vertexCount, vertex);
          arcCount <- Gen.choose(arcMinCount, vertexCount * (vertexCount - 1) / 2);
          as <- Gen.containerOfN[Set, Arc[Int]](arcCount, arcFrom(vs))) yield StrictGraphMatrixImpl(vs.toSeq, matrixFromVerticesAndArcs[Int](vs.toSeq, as))
  }