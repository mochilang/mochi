use std::io::{self, Read};

struct MedianFinder {
    data: Vec<i32>,
}

impl MedianFinder {
    fn new() -> Self {
        Self { data: Vec::new() }
    }

    fn add_num(&mut self, num: i32) {
        let pos = self.data.binary_search(&num).unwrap_or_else(|p| p);
        self.data.insert(pos, num);
    }

    fn find_median(&self) -> f64 {
        let n = self.data.len();
        if n % 2 == 1 {
            self.data[n / 2] as f64
        } else {
            (self.data[n / 2 - 1] + self.data[n / 2]) as f64 / 2.0
        }
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().map(|s| s.trim()).filter(|s| !s.is_empty()).collect();
    if lines.is_empty() {
        return;
    }
    let t: usize = lines[0].parse().unwrap();
    let mut idx = 1;
    let mut blocks: Vec<String> = Vec::new();
    for _ in 0..t {
        let m: usize = lines[idx].parse().unwrap();
        idx += 1;
        let mut mf = MedianFinder::new();
        let mut out: Vec<String> = Vec::new();
        for _ in 0..m {
            let parts: Vec<&str> = lines[idx].split_whitespace().collect();
            idx += 1;
            if parts[0] == "addNum" {
                mf.add_num(parts[1].parse().unwrap());
            } else {
                out.push(format!("{:.1}", mf.find_median()));
            }
        }
        blocks.push(out.join("\n"));
    }
    print!("{}", blocks.join("\n\n"));
}
