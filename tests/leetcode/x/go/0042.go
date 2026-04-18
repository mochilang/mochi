package main

import (
  "bufio"
  "fmt"
  "os"
  "strconv"
  "strings"
)

func trap(height []int) int {
  left, right := 0, len(height)-1
  leftMax, rightMax, water := 0, 0, 0
  for left <= right {
    if leftMax <= rightMax {
      if height[left] < leftMax { water += leftMax - height[left] } else { leftMax = height[left] }
      left++
    } else {
      if height[right] < rightMax { water += rightMax - height[right] } else { rightMax = height[right] }
      right--
    }
  }
  return water
}

func main() {
  scanner := bufio.NewScanner(os.Stdin)
  lines := []string{}
  for scanner.Scan() { lines = append(lines, strings.TrimSpace(scanner.Text())) }
  if len(lines) == 0 || lines[0] == "" { return }
  idx := 0
  t, _ := strconv.Atoi(lines[idx]); idx++
  out := make([]string, 0, t)
  for tc := 0; tc < t; tc++ {
    n, _ := strconv.Atoi(lines[idx]); idx++
    arr := make([]int, n)
    for i := 0; i < n; i++ { arr[i], _ = strconv.Atoi(lines[idx]); idx++ }
    out = append(out, strconv.Itoa(trap(arr)))
  }
  fmt.Print(strings.Join(out, "\n"))
}
