fun main() {
	var data = mapOf("outer" to mapOf("inner" to 1))
	data["outer"]["inner"] = 2
	println(data["outer"]["inner"])
}
