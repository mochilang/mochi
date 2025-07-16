object case_sensitivity_of_identifiers {
  def main() = {
    var pkg_dog = "Salt"
    var Dog = "Pepper"
    var pkg_DOG = "Mustard"
    def packageSees(d1: String, d2: String, d3: String): Map[String, Boolean] = {
      println("Package sees: " + d1 + " " + d2 + " " + d3)
      return Map("pkg_dog" -> true, "Dog" -> true, "pkg_DOG" -> true)
    }
    var d = packageSees(pkg_dog, Dog, pkg_DOG)
    println("There are " + d.length.toString + " dogs.\n")
    var dog = "Benjamin"
    d = packageSees(pkg_dog, Dog, pkg_DOG)
    println("Main sees:   " + dog + " " + Dog + " " + pkg_DOG)
    d("dog") = true
    d("Dog") = true
    d("pkg_DOG") = true
    println("There are " + d.length.toString + " dogs.\n")
    Dog = "Samba"
    d = packageSees(pkg_dog, Dog, pkg_DOG)
    println("Main sees:   " + dog + " " + Dog + " " + pkg_DOG)
    d("dog") = true
    d("Dog") = true
    d("pkg_DOG") = true
    println("There are " + d.length.toString + " dogs.\n")
    var DOG = "Bernie"
    d = packageSees(pkg_dog, Dog, pkg_DOG)
    println("Main sees:   " + dog + " " + Dog + " " + DOG)
    d("dog") = true
    d("Dog") = true
    d("pkg_DOG") = true
    d("DOG") = true
    println("There are " + d.length.toString + " dogs.")
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
