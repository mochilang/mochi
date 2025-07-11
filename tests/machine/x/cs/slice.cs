using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        Console.WriteLine("[" + string.Join(", ", _sliceList(new List<int> { 1, 2, 3 }, 1, 3)) + "]");
        Console.WriteLine("[" + string.Join(", ", _sliceList(new List<int> { 1, 2, 3 }, 0, 2)) + "]");
        Console.WriteLine(_sliceString("hello", 1, 4));
    }
    static List<dynamic> _sliceList(dynamic l, long i, long j) {
        var list = l as System.Collections.IList;
        if (list == null) return new List<dynamic>();
        var start = i;
        var end = j;
        var n = list.Count;
        if (start < 0) start += n;
        if (end < 0) end += n;
        if (start < 0) start = 0;
        if (end > n) end = n;
        if (end < start) end = start;
        var res = new List<dynamic>();
        for (int k = (int)start; k < (int)end; k++) res.Add(list[k]);
        return res;
    }
    
    static string _sliceString(string s, long i, long j) {
        var start = i;
        var end = j;
        var n = s.Length;
        if (start < 0) start += n;
        if (end < 0) end += n;
        if (start < 0) start = 0;
        if (end > n) end = n;
        if (end < start) end = start;
        return s.Substring((int)start, (int)(end - start));
    }
    
}
