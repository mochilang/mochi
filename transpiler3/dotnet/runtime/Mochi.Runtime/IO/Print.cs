namespace Mochi.Runtime.IO;

using System.Globalization;

/// <summary>Scalar print helpers. Formatting matches Mochi vm3 (Go) output.</summary>
public static class Print
{
    public static void Line(string v) => Console.WriteLine(v);
    public static void Line(long v) => Console.WriteLine(v);

    public static void Line(double v)
    {
        string s = double.IsNaN(v) ? "NaN"
                 : double.IsPositiveInfinity(v) ? "+Inf"
                 : double.IsNegativeInfinity(v) ? "-Inf"
                 : v.ToString("G", CultureInfo.InvariantCulture);
        Console.WriteLine(s);
    }

    public static void Line(bool v) => Console.WriteLine(v ? "true" : "false");
    public static void Line(object v) => Console.WriteLine(v);
}
