import 'dart:io';
import 'dart:convert';

class Solution {
  int removeDuplicates(List<int> nums) {
    if (nums.isEmpty) return 0;
    int k = 1;
    for (int i = 1; i < nums.length; i++) {
      if (nums[i] != nums[k - 1]) {
        nums[k] = nums[i];
        k++;
      }
    }
    return k;
  }
}

void main() async {
  String input = await stdin.transform(utf8.decoder).join();
  List<String> tokens = input.split(RegExp(r'\s+')).where((s) => s.isNotEmpty).toList();
  if (tokens.isEmpty) return;
  int idx = 0;
  int t = int.parse(tokens[idx++]);
  Solution sol = Solution();
  while (t-- > 0) {
    int n = int.parse(tokens[idx++]);
    List<int> nums = [];
    for (int i = 0; i < n; i++) {
      nums.add(int.parse(tokens[idx++]));
    }
    int k = sol.removeDuplicates(nums);
    print(nums.take(k).join(" "));
  }
}
