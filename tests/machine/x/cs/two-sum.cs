using System;
using System.Collections.Generic;

class Program {
    static List<int> twoSum(List<int> nums, int target) {
        int n = nums.Length;
        for (var i = 0; i < n; i++) {
            for (var j = (i + 1); j < n; j++) {
                if ((_indexList(nums, i) + _indexList(nums, j)) == target) {
                    return new List<dynamic> { i, j };
                }
            }
        }
        return new List<int> { (-1), (-1) };
    }
    
    static void Main() {
        List<int> result = twoSum(new List<int> { 2, 7, 11, 15 }, 9);
        Console.WriteLine(_indexList(result, 0));
        Console.WriteLine(_indexList(result, 1));
    }
    static dynamic _indexList(dynamic l, long i) {
        var list = l as System.Collections.IList;
        if (list == null) throw new Exception("index() expects list");
        if (i < 0) i += list.Count;
        if (i < 0 || i >= list.Count) throw new Exception("index out of range");
        return list[(int)i];
    }
    
}
