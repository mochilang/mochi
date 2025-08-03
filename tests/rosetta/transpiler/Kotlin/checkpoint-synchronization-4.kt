var nMech: Int = 5
var detailsPerMech: Int = 4
fun main() {
    for (mech in 1 until nMech + 1) {
        var id: Int = mech
        println(((("worker " + id.toString()) + " contracted to assemble ") + detailsPerMech.toString()) + " details")
        println(("worker " + id.toString()) + " enters shop")
        var d: Int = 0
        while (d < detailsPerMech) {
            println(("worker " + id.toString()) + " assembling")
            println(("worker " + id.toString()) + " completed detail")
            d = d + 1
        }
        println(("worker " + id.toString()) + " leaves shop")
        println(("mechanism " + mech.toString()) + " completed")
    }
}
