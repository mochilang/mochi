using System;

class Program
{
    static void Main()
    {
        int[][] matrix = { new[] { 1, 2 }, new[] { 3, 4 } };
        matrix[1][0] = 5;
        Console.WriteLine(matrix[1][0]);
    }
}
