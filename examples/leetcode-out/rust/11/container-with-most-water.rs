fn maxArea(height: Vec<i32>) -> i32 {
    let mut left = 0;
    let mut right = height.len() as i32 - 1;
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

fn main() {
}

