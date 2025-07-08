fn main() {
    let mut matrix = vec![vec![1, 2], vec![3, 4]];
    matrix[1 as usize][0 as usize] = 5;
    println!("{:?}", matrix[1 as usize][0 as usize]);
}
