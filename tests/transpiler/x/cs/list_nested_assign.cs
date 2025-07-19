// Mochi 0.10.31 - generated 2025-07-19 10:19:08 UTC
using System;

class Program {
    static void Main() {
        var matrix = new[]{new[]{1, 2}, new[]{3, 4}};
        matrix[1][0] = 5;
        Console.WriteLine(matrix[1][0]);
    }
}
