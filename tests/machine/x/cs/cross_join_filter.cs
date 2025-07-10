using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        long[] nums = new long[] { 1, 2, 3 };
        string[] letters = new string[] { "A", "B" };
        var pairs = new Func<List<Dictionary<string, dynamic>>>(() =>
        {
            var _res = new List<Dictionary<string, dynamic>>();
            foreach (var n in nums)
            {
                if (!(((n % 2) == 0))) continue;
                foreach (var l in letters)
                {
                    if (((n % 2) == 0))
                    {
                        _res.Add(new Dictionary<string, dynamic> { { "n", n }, { "l", l } });
                    }
                }
            }
            return _res;
        })();
        Console.WriteLine("--- Even pairs ---");
        foreach (var p in pairs)
        {
            Console.WriteLine(string.Join(" ", new[] { Convert.ToString(p["n"]), Convert.ToString(p["l"]) }));
        }
    }
}
