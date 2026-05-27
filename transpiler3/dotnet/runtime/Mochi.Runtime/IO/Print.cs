namespace Mochi.Runtime.IO;

/// <summary>Scalar print helpers. bool uses "true"/"false" to match Mochi vm3 output.</summary>
public static class Print
{
    public static void Line(string v) => Console.WriteLine(v);
    public static void Line(long v) => Console.WriteLine(v);
    public static void Line(double v) =>
        Console.WriteLine(v.ToString("G", System.Globalization.CultureInfo.InvariantCulture));
    public static void Line(bool v) => Console.WriteLine(v ? "true" : "false");
    public static void Line(object v) => Console.WriteLine(v);
}
