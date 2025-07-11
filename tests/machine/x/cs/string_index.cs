using System;

class Program {
    static void Main() {
        string s = "mochi";
        Console.WriteLine(_indexString(s, 1));
    }
    static string _indexString(string s, long i) {
        if (i < 0) i += s.Length;
        if (i < 0 || i >= s.Length) throw new Exception("index out of range");
        return s[(int)i].ToString();
    }
    
}
