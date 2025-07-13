List<int> remove(List<int> nums, int i) {
  return (nums.sublist(0, i) + nums.sublist((i + 1), nums.length));
}

void main() {
  print(remove([1, 2, 3, 4], 1));
}
