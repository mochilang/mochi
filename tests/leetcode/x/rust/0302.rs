use std::io::{self, Read};

fn min_area(image: &[String], _x: i32, _y: i32) -> i32 {
    let mut top = image.len() as i32;
    let mut bottom = -1;
    let mut left = image[0].len() as i32;
    let mut right = -1;
    for (i, row) in image.iter().enumerate() {
        for (j, ch) in row.chars().enumerate() {
            if ch == '1' {
                top = top.min(i as i32);
                bottom = bottom.max(i as i32);
                left = left.min(j as i32);
                right = right.max(j as i32);
            }
        }
    }
    (bottom - top + 1) * (right - left + 1)
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().map(|s| s.trim()).filter(|s| !s.is_empty()).collect();
    if lines.is_empty() {
        return;
    }
    let t: usize = lines[0].parse().unwrap();
    let mut idx = 1usize;
    let mut out = Vec::new();
    for _ in 0..t {
        let parts: Vec<&str> = lines[idx].split_whitespace().collect();
        idx += 1;
        let r: usize = parts[0].parse().unwrap();
        let mut image = Vec::new();
        for _ in 0..r {
            image.push(lines[idx].to_string());
            idx += 1;
        }
        let parts: Vec<&str> = lines[idx].split_whitespace().collect();
        idx += 1;
        let x: i32 = parts[0].parse().unwrap();
        let y: i32 = parts[1].parse().unwrap();
        out.push(min_area(&image, x, y).to_string());
    }
    print!("{}", out.join("\n\n"));
}
