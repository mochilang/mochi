using System;

class Program
{
    static void Main()
    {
        long[][] matrix = new long[][] { new long[] { 1, 2 }, new long[] { 3, 4 } };
        matrix[1][0] = 5;
        Console.WriteLine(_indexList(_indexList(matrix, 1), 0));
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
