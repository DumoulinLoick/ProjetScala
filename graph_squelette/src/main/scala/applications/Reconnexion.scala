package applications

object Reconnexion extends App {

    val antennes = Reader.getAntennes()
    println(s"${antennes.size} antennes")

}
