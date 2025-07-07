using System;
using System.Linq;
using System.Collections.Generic;
class Program{
    static void Main(){
        int[] xs={1,2,3};
        var ys=xs.Where(x=>x%2==1);
        Console.WriteLine(ys.Contains(1));
        Console.WriteLine(ys.Contains(2));
        var m=new Dictionary<string,int>{{"a",1}};
        Console.WriteLine(m.ContainsKey("a"));
        Console.WriteLine(m.ContainsKey("b"));
        string s="hello";
        Console.WriteLine(s.Contains("ell"));
        Console.WriteLine(s.Contains("foo"));
    }
}
