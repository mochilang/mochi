using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        List<List<int>> matrix = new List<List<int>> { new List<int> { 1, 2 }, new List<int> { 3, 4 } };
        matrix[1][0] = 5;
        Console.WriteLine(_indexList(_indexList(matrix, 1), 0));
    }
    static dynamic _indexList(dynamic l, long i) {
        var list = l as System.Collections.IList;
        if (list == null) throw new Exception("index() expects list");
        if (i < 0) i += list.Count;
        if (i < 0 || i >= list.Count) throw new Exception("index out of range");
        return list[(int)i];
    }
    
}
