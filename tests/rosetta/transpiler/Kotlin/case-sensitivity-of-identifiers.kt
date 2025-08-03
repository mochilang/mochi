fun user_main(): Unit {
    var pkg_dog: String = "Salt"
    var Dog: String = "Pepper"
    var pkg_DOG: String = "Mustard"
    fun packageSees(d1: String, d2: String, d3: String): MutableMap<String, Boolean> {
        println((((("Package sees: " + d1) + " ") + d2) + " ") + d3)
        return mutableMapOf<String, Boolean>("pkg_dog" to (true), "Dog" to (true), "pkg_DOG" to (true))
    }

    var d: MutableMap<String, Boolean> = packageSees(pkg_dog, Dog, pkg_DOG)
    println(("There are " + d.size.toString()) + " dogs.\n")
    var dog: String = "Benjamin"
    d = ((packageSees(pkg_dog, Dog, pkg_DOG)) as MutableMap<String, Boolean>)
    println((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG)
    (d)["dog"] = true
    (d)["Dog"] = true
    (d)["pkg_DOG"] = true
    println(("There are " + d.size.toString()) + " dogs.\n")
    Dog = "Samba"
    d = ((packageSees(pkg_dog, Dog, pkg_DOG)) as MutableMap<String, Boolean>)
    println((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG)
    (d)["dog"] = true
    (d)["Dog"] = true
    (d)["pkg_DOG"] = true
    println(("There are " + d.size.toString()) + " dogs.\n")
    var DOG: String = "Bernie"
    d = ((packageSees(pkg_dog, Dog, pkg_DOG)) as MutableMap<String, Boolean>)
    println((((("Main sees:   " + dog) + " ") + Dog) + " ") + DOG)
    (d)["dog"] = true
    (d)["Dog"] = true
    (d)["pkg_DOG"] = true
    (d)["DOG"] = true
    println(("There are " + d.size.toString()) + " dogs.")
}

fun main() {
    user_main()
}
