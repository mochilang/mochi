// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fn main() {
    fn main() -> () {
        let mut pkg_dog = String::from("Salt");
        let mut Dog = String::from("Pepper");
        let mut pkg_DOG = String::from("Mustard");
        let packageSees = move |d1: &'static str, d2: &'static str, d3: &'static str| -> std::collections::HashMap<&'static str, bool> {
            println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "Package sees: ", d1), " "), d2), " "), d3))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
            return { let mut m = std::collections::BTreeMap::new(); m.insert("pkg_dog", true); m.insert("Dog", true); m.insert("pkg_DOG", true); m };
        };
        let mut d = packageSees(pkg_dog, Dog, pkg_DOG);
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", "There are ", d.len() as i32.to_string()), " dogs.
"))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        let mut dog = String::from("Benjamin");
        d = packageSees(pkg_dog, Dog, pkg_DOG);
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "Main sees:   ", dog), " "), Dog), " "), pkg_DOG))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        d["dog" as usize] = true;
        d["Dog" as usize] = true;
        d["pkg_DOG" as usize] = true;
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", "There are ", d.len() as i32.to_string()), " dogs.
"))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        Dog = "Samba";
        d = packageSees(pkg_dog, Dog, pkg_DOG);
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "Main sees:   ", dog), " "), Dog), " "), pkg_DOG))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        d["dog" as usize] = true;
        d["Dog" as usize] = true;
        d["pkg_DOG" as usize] = true;
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", "There are ", d.len() as i32.to_string()), " dogs.
"))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        let mut DOG = String::from("Bernie");
        d = packageSees(pkg_dog, Dog, pkg_DOG);
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "Main sees:   ", dog), " "), Dog), " "), DOG))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        d["dog" as usize] = true;
        d["Dog" as usize] = true;
        d["pkg_DOG" as usize] = true;
        d["DOG" as usize] = true;
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", "There are ", d.len() as i32.to_string()), " dogs."))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    }
    main();
}
