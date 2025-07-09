fn main() {
    let x = 2;
    let label = match x {1 => "one", 2 => "two", 3 => "three", _ => "unknown", };
    println!("{:?}", label);
    let day = "sun";
    let mood = match day {"mon" => "tired", "fri" => "excited", "sun" => "relaxed", _ => "normal", };
    println!("{:?}", mood);
    let ok = true;
    let status = match ok {true => "confirmed", false => "denied", };
    println!("{:?}", status);
    fn classify(n: i32) -> &'static str {
        return match n {0 => "zero", 1 => "one", _ => "many", };
    }
    println!("{:?}", classify(0));
    println!("{:?}", classify(5));
}
