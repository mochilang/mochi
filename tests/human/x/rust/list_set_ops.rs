use std::collections::HashSet;

fn main() {
    let union: HashSet<i32> = [1, 2].iter().cloned().chain([2, 3].iter().cloned()).collect();
    println!("{:?}", union);
    let except: Vec<i32> = vec![1, 2, 3].into_iter().filter(|&x| x != 2).collect();
    println!("{:?}", except);
    let intersect: Vec<i32> = vec![1, 2, 3].into_iter().filter(|x| [2, 4].contains(x)).collect();
    println!("{:?}", intersect);
    let union_all_len = [1, 2].len() + [2, 3].len();
    println!("{}", union_all_len);
}
