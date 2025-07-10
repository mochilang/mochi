using System;

class Program
{
    static void Main()
    {
        long[] xs = new long[] { 10, 20, 30 };
        Console.WriteLine(_indexList(xs, 1));
    }
    static dynamic _indexList(dynamic l, long i)
    {
        var list = l as System.Collections.IList;
        if (list == null) throw new Exception("index() expects list");
        if (i < 0) i += list.Count;
        if (i < 0 || i >= list.Count) throw new Exception("index out of range");
        return list[(int)i];
    }

}
