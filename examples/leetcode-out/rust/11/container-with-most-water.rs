fn maxArea(height: Vec<i64>) -> i64 {
    let mut left = 0;
    let mut right = height.len() as i64 - 1;
    let mut maxArea = 0;
    while left < right {
        let mut width = right - left;
        let mut h = 0;
        if height[left as usize] < height[right as usize] {
            h = height[left as usize];
        } else {
            h = height[right as usize];
        }
        let mut area = h * width;
        if area > maxArea {
            maxArea = area;
        }
        if height[left as usize] < height[right as usize] {
            left = left + 1;
        } else {
            right = right - 1;
        }
    }
    return maxArea;
}

fn test_example_1() {
    expect(maxArea(vec![1, 8, 6, 2, 5, 4, 8, 3, 7]) == 49);
}

fn test_example_2() {
    expect(maxArea(vec![1, 1]) == 1);
}

fn test_decreasing_heights() {
    expect(maxArea(vec![4, 3, 2, 1, 4]) == 16);
}

fn test_short_array() {
    expect(maxArea(vec![1, 2, 1]) == 2);
}

fn main() {
    test_example_1();
    test_example_2();
    test_decreasing_heights();
    test_short_array();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
