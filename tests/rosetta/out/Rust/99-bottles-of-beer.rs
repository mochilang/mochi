// Generated by Mochi compiler v0.10.30 on 2025-07-19T02:23:53Z
fn main() {
    let bottles = move |n: i32| -> String {
        if n == 0 {
            return String::from("No more bottles");
        }
        if n == 1 {
            return String::from("1 bottle");
        }
        return format!("{}{}", n.to_string(), String::from(" bottles"));
    };
    let main = move || -> () {
        let mut i = 99;
        while i > 0 {
            println!("{}", vec![format!("{}", format!("{}{}", bottles(i), String::from(" of beer on the wall")))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
            println!("{}", vec![format!("{}", format!("{}{}", bottles(i), String::from(" of beer")))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
            println!("Take one down, pass it around");
            println!("{}", vec![format!("{}", format!("{}{}", bottles(i - 1), String::from(" of beer on the wall")))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
            i -= 1;
        }
    };
    main();
}
