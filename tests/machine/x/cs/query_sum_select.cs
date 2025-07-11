using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<int> nums = new List<int> { 1, 2, 3 };
        List<int> result = nums.Where(n => (n > 1)).Select(n => _sum(n)).ToArray();
        Console.WriteLine("[" + string.Join(", ", result) + "]");
    }
    static double _sum(dynamic v) {
        if (v == null) return 0.0;
        double _sum = 0;
        foreach (var it in v) {
            _sum += Convert.ToDouble(it);
        }
        return _sum;
    }
    
}
