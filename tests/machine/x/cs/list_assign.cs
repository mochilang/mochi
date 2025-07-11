using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        List<int> nums = new List<int> { 1, 2 };
        nums[1] = 3;
        Console.WriteLine(_indexList(nums, 1));
    }
    static dynamic _indexList(dynamic l, long i) {
        var list = l as System.Collections.IList;
        if (list == null) throw new Exception("index() expects list");
        if (i < 0) i += list.Count;
        if (i < 0 || i >= list.Count) throw new Exception("index out of range");
        return list[(int)i];
    }
    
}
