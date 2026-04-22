using System;
using System.Collections.Generic;

public class Program
{
    static readonly string[] Less20 = { "", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten",
        "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen" };
    static readonly string[] Tens = { "", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety" };
    static readonly string[] Thousands = { "", "Thousand", "Million", "Billion" };

    static string Helper(int n)
    {
        if (n == 0) return "";
        if (n < 20) return Less20[n];
        if (n < 100) return Tens[n / 10] + (n % 10 == 0 ? "" : " " + Helper(n % 10));
        return Less20[n / 100] + " Hundred" + (n % 100 == 0 ? "" : " " + Helper(n % 100));
    }

    static string Solve(int num)
    {
        if (num == 0) return "Zero";
        var parts = new List<string>();
        int idx = 0;
        while (num > 0)
        {
            int chunk = num % 1000;
            if (chunk != 0)
            {
                string words = Helper(chunk);
                if (Thousands[idx] != "") words += " " + Thousands[idx];
                parts.Add(words);
            }
            num /= 1000;
            idx++;
        }
        parts.Reverse();
        return string.Join(" ", parts);
    }

    public static void Main()
    {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var outLines = new List<string>();
        for (int i = 0; i < t; i++) outLines.Add(Solve(int.Parse((Console.ReadLine() ?? "0").Trim())));
        Console.Write(string.Join("\n", outLines));
    }
}
