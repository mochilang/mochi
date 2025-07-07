using System;
using System.Linq;

class Program {
    static void Main() {
        var list1 = new[] {1, 2};
        var list2 = new[] {2, 3};
        Console.WriteLine("[" + string.Join(",", list1.Union(list2)) + "]");
        Console.WriteLine("[" + string.Join(",", new[]{1,2,3}.Except(new[]{2})) + "]");
        Console.WriteLine("[" + string.Join(",", new[]{1,2,3}.Intersect(new[]{2,4})) + "]");
        var lenUnionAll = new[]{1,2}.Concat(new[]{2,3}).Count();
        Console.WriteLine(lenUnionAll);
    }
}
