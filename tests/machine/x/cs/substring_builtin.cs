using System;

class Program {
    static void Main() {
        Console.WriteLine(_sliceString("mochi", 1, 4));
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
