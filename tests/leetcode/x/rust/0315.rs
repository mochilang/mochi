use std::io::{self, Read};

fn sort(nums: &[i32], idx: &mut [usize], tmp: &mut [usize], counts: &mut [i32], lo: usize, hi: usize) {
    if hi - lo <= 1 {
        return;
    }
    let mid = (lo + hi) / 2;
    sort(nums, idx, tmp, counts, lo, mid);
    sort(nums, idx, tmp, counts, mid, hi);
    let (mut i, mut j, mut k, mut moved) = (lo, mid, lo, 0);
    while i < mid && j < hi {
        if nums[idx[j]] < nums[idx[i]] {
            tmp[k] = idx[j];
            j += 1;
            moved += 1;
        } else {
            counts[idx[i]] += moved;
            tmp[k] = idx[i];
            i += 1;
        }
        k += 1;
    }
    while i < mid {
        counts[idx[i]] += moved;
        tmp[k] = idx[i];
        i += 1;
        k += 1;
    }
    while j < hi {
        tmp[k] = idx[j];
        j += 1;
        k += 1;
    }
    idx[lo..hi].copy_from_slice(&tmp[lo..hi]);
}

fn count_smaller(nums: &[i32]) -> Vec<i32> {
    let n = nums.len();
    let mut counts = vec![0; n];
    let mut idx: Vec<usize> = (0..n).collect();
    let mut tmp = vec![0; n];
    sort(nums, &mut idx, &mut tmp, &mut counts, 0, n);
    counts
}

fn fmt_list(a: &[i32]) -> String {
    format!("[{}]", a.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(","))
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let data: Vec<i32> = input.split_whitespace().filter_map(|s| s.parse().ok()).collect();
    if data.is_empty() {
        return;
    }
    let mut pos = 0usize;
    let t = data[pos] as usize;
    pos += 1;
    let mut out = Vec::new();
    for _ in 0..t {
        let n = data[pos] as usize;
        pos += 1;
        let nums = data[pos..pos + n].to_vec();
        pos += n;
        out.push(fmt_list(&count_smaller(&nums)));
    }
    print!("{}", out.join("\n\n"));
}
