package application

import scala.annotation.tailrec
import undirected.SimpleGraphNeighborsImpl

object test {
  def main(args: Array[String]) = {

    val yuki: SimpleGraphNeighborsImpl[Int]=SimpleGraphNeighborsImpl[Int](Map(0->Set(1,4,5,7),1->Set(0,2,4,6),2->Set(1,3),3->Set(2,4,6),4->Set(0,1,3,5,7),5->Set(0,4,7),6->Set(1,3,7),7->Set(0,4,5,6)))
    
    println(yuki)
  }
}

