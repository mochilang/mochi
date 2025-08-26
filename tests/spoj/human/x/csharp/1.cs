// https://www.spoj.com/problems/TEST/
using System;

public class Program
{
    public static void Main()
    {
        string line;
        while ((line = Console.ReadLine()) != null)
        {
            int n;
            if (int.TryParse(line, out n))
            {
                if (n == 42)
                    break;
                Console.WriteLine(n);
            }
        }
    }
}
