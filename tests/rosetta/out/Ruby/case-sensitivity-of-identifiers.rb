def main()
	pkg_dog = "Salt"
	Dog = "Pepper"
	pkg_DOG = "Mustard"
	packageSees = ->(d1, d2, d3){
		puts(((((("Package sees: " + d1) + " ") + d2) + " ") + d3))
		return {"pkg_dog" => true, "Dog" => true, "pkg_DOG" => true}
	}
	d = packageSees.call(pkg_dog, Dog, pkg_DOG)
	puts((("There are " + ((d).length).to_s) + " dogs.\n"))
	dog = "Benjamin"
	d = packageSees.call(pkg_dog, Dog, pkg_DOG)
	puts(((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG))
	d["dog"] = true
	d["Dog"] = true
	d["pkg_DOG"] = true
	puts((("There are " + ((d).length).to_s) + " dogs.\n"))
	Dog = "Samba"
	d = packageSees.call(pkg_dog, Dog, pkg_DOG)
	puts(((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG))
	d["dog"] = true
	d["Dog"] = true
	d["pkg_DOG"] = true
	puts((("There are " + ((d).length).to_s) + " dogs.\n"))
	DOG = "Bernie"
	d = packageSees.call(pkg_dog, Dog, pkg_DOG)
	puts(((((("Main sees:   " + dog) + " ") + Dog) + " ") + DOG))
	d["dog"] = true
	d["Dog"] = true
	d["pkg_DOG"] = true
	d["DOG"] = true
	puts((("There are " + ((d).length).to_s) + " dogs."))
end

main()
