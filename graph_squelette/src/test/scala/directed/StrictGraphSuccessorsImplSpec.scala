package directed

import org.scalacheck.Gen
import org.scalacheck.Gen._

/** The test class for [[StrictGraphDefaultImpl]] implementation */
class StrictGraphSuccessorsImplSpec extends StrictGraphSpec(StrictGraphSuccessorsImplSpec)

object StrictGraphSuccessorsImplSpec extends StrictGraphSpecCompanion[Int]("StrictGraphSuccessorsImpl") {
   /** @inheritdoc */
   val vertex : Gen[Int] = posNum[Int]

   def SuccessorsFromVerticesAndArcs[V](vertices: Set[V], arcs: Set[Arc[V]]): Map[V, Set[V]] = 
     vertices.foldLeft(Map.empty[V, Set[V]]){(acc, x) => acc + (x -> arcs.filter((ac:Arc[V]) => ac._1 == x).map(a => a._2))}

   /** @inheritdoc */
   def graphWithAtLeast(vertexMinCount: Int, arcMinCount: Int = 0): Gen[StrictGraphSuccessorsImpl[Int]] =
     for(vertexAdditionalCount <- posNum[Int]; vertexCount <- Gen.const((vertexMinCount + vertexAdditionalCount) max 1);
         vs <- Gen.containerOfN[Set, Int](vertexCount, vertex);
         arcCount <- Gen.choose(arcMinCount, vertexCount * (vertexCount - 1) / 2);
         as <- Gen.containerOfN[Set, Arc[Int]](arcCount, arcFrom(vs))) yield StrictGraphSuccessorsImpl(SuccessorsFromVerticesAndArcs[Int](vs, as))
 }
 
