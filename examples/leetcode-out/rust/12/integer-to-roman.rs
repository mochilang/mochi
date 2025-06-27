fn intToRoman(mut num: i64) -> String {
    let mut values = vec![1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
    let mut symbols = vec![
        "M".to_string(),
        "CM".to_string(),
        "D".to_string(),
        "CD".to_string(),
        "C".to_string(),
        "XC".to_string(),
        "L".to_string(),
        "XL".to_string(),
        "X".to_string(),
        "IX".to_string(),
        "V".to_string(),
        "IV".to_string(),
        "I".to_string(),
    ];
    let mut result = "".to_string();
    let mut i = 0;
    while num > 0 {
        while num >= values[i as usize] {
            result = format!("{}{}", result, symbols[i as usize]);
            num = num - values[i as usize];
        }
        i = i + 1;
    }
    return result.to_string();
}

fn test_example_1() {
    expect(intToRoman(3) == "III");
}

fn test_example_2() {
    expect(intToRoman(58) == "LVIII");
}

fn test_example_3() {
    expect(intToRoman(1994) == "MCMXCIV");
}

fn test_small_numbers() {
    expect(intToRoman(4) == "IV");
    expect(intToRoman(9) == "IX");
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_small_numbers();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
