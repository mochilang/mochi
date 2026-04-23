use std::io::{self, Read};

fn count_range_sum(nums: &[i64], lower: i64, upper: i64) -> i32 {
    let mut pref = Vec::with_capacity(nums.len() + 1);
    pref.push(0);
    for &x in nums {
        let last = *pref.last().unwrap();
        pref.push(last + x);
    }
    let mut tmp = pref.clone();
    fn sort(pref: &mut [i64], tmp: &mut [i64], lo: usize, hi: usize, lower: i64, upper: i64) -> i32 {
        if hi - lo <= 1 { return 0; }
        let mid = (lo + hi) / 2;
        let mut ans = sort(pref, tmp, lo, mid, lower, upper) + sort(pref, tmp, mid, hi, lower, upper);
        let mut left = lo;
        let mut right = lo;
        for r in mid..hi {
            while left < mid && pref[left] < pref[r] - upper { left += 1; }
            while right < mid && pref[right] <= pref[r] - lower { right += 1; }
            ans += (right - left) as i32;
        }
        let (mut i, mut j, mut k) = (lo, mid, lo);
        while i < mid && j < hi {
            if pref[i] <= pref[j] { tmp[k] = pref[i]; i += 1; } else { tmp[k] = pref[j]; j += 1; }
            k += 1;
        }
        while i < mid { tmp[k] = pref[i]; i += 1; k += 1; }
        while j < hi { tmp[k] = pref[j]; j += 1; k += 1; }
        pref[lo..hi].copy_from_slice(&tmp[lo..hi]);
        ans
    }
    let n = pref.len();
    sort(&mut pref, &mut tmp, 0, n, lower, upper)
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let data: Vec<i64> = s.split_whitespace().map(|x| x.parse().unwrap()).collect();
    if data.is_empty() { return; }
    let mut idx = 0;
    let t = data[idx] as usize; idx += 1;
    let mut out = Vec::new();
    for _ in 0..t {
        let n = data[idx] as usize; idx += 1;
        let nums = data[idx..idx+n].to_vec(); idx += n;
        let lower = data[idx]; let upper = data[idx+1]; idx += 2;
        out.push(count_range_sum(&nums, lower, upper).to_string());
    }
    print!("{}", out.join("\n\n"));
}
