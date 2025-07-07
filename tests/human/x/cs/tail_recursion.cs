using System;

class Program
{
    static int SumRec(int n, int acc)
    {
        if (n == 0)
            return acc;
        return SumRec(n - 1, acc + n);
    }

    static void Main()
    {
        Console.WriteLine(SumRec(10, 0));
    }
}
