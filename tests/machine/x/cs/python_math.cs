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
        double r = 3.000000;
        double area = (math.pi * math.pow(r, 2.000000));
        double root = math.sqrt(49.000000);
        double sin45 = math.sin((math.pi / 4.000000));
        double log_e = math.log(math.e);
        Console.WriteLine(string.Join(" ", new[] { Convert.ToString("Circle area with r ="), Convert.ToString(r), Convert.ToString("=>"), Convert.ToString(area) }));
        Console.WriteLine(string.Join(" ", new[] { Convert.ToString("Square root of 49:"), Convert.ToString(root) }));
        Console.WriteLine(string.Join(" ", new[] { Convert.ToString("sin(π/4):"), Convert.ToString(sin45) }));
        Console.WriteLine(string.Join(" ", new[] { Convert.ToString("log(e):"), Convert.ToString(log_e) }));
    }
}
