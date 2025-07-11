using System;

static class math
{
    public const double pi = Math.PI;
    public const double e = Math.E;
    public static double sqrt(double x) { return Math.Sqrt(x); }
    public static double pow(double x, double y) { return Math.Pow(x, y); }
    public static double sin(double x) { return Math.Sin(x); }
    public static double log(double x) { return Math.Log(x); }
}

class Program
{
    static void Main()
    {
        Console.WriteLine(math.sqrt(16.000000));
        Console.WriteLine(math.pi);
    }
}
