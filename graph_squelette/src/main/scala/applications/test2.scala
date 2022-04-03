package application

import scala.annotation.tailrec
import directed.StrictGraphMatrixImpl

object test2 {
  def main(args: Array[String]) = {
    val reimu:IndexedSeq[IndexedSeq[Int]]=IndexedSeq(IndexedSeq(0,0,4,0,5,0,0),IndexedSeq(0,0,2,4,0,3,1),IndexedSeq(4,2,0,0,2,0,0),IndexedSeq(0,4,0,0,0,1,3),IndexedSeq(5,0,2,0,0,0,5),IndexedSeq(0,3,0,1,0,0,0),IndexedSeq(0,1,0,3,5,0,0))
    val marisa:IndexedSeq[IndexedSeq[Int]]=IndexedSeq(IndexedSeq(0,0,1,0,1,0,0),IndexedSeq(0,0,1,1,0,1,1),IndexedSeq(1,1,0,0,1,0,0),IndexedSeq(0,1,0,0,0,1,1),IndexedSeq(1,0,1,0,0,0,1),IndexedSeq(0,1,0,1,0,0,0),IndexedSeq(0,1,0,1,1,0,0))

    val yuki: StrictGraphMatrixImpl[Int]=StrictGraphMatrixImpl(Seq[Int](0,1,2,3,4,5,6),reimu)            
    val gnocid: StrictGraphMatrixImpl[Int]=StrictGraphMatrixImpl(Seq[Int](0,1,2,3,4,5,6),reimu)    
    println(yuki.shortestPath(yuki.valuation(yuki.vertices,yuki.arcs.toList,gnocid.adjacency))(1,3))    
    val matrice:IndexedSeq[IndexedSeq[Int]]={IndexedSeq( IndexedSeq(0,1,0,0,0,0),
                                                        IndexedSeq(1,0,1,0,0,0),
                                                        IndexedSeq(0,1,0,1,0,0),
                                                        IndexedSeq(0,0,1,0,1,0),
                                                        IndexedSeq(0,0,0,1,0,1),
                                                        IndexedSeq(0,0,0,0,1,0) )
                                        
                                            }
    val testest: StrictGraphMatrixImpl[Int]=StrictGraphMatrixImpl(Seq[Int](0,1,2,3,4,5),matrice)            
    println(testest.shortestPath(testest.valuation(testest.vertices,testest.arcs.toList,testest.adjacency))(0,5))  
    println(testest.valuation(testest.vertices,testest.arcs.toList,testest.adjacency))  

  }
}
