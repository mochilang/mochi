using System;
using System.Linq;

class Program {
    static void Main() {
        long[] nums = new long[] { 1, 2, 3 };
        double[] result = nums.Where(n => (n > 1)).Select(n => _sum(n)).ToArray();
        Console.WriteLine(result);
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
