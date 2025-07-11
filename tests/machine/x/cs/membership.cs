using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<int> nums = new List<int> { 1, 2, 3 };
        Console.WriteLine(nums.Contains(2));
        Console.WriteLine(nums.Contains(4));
    }
}
