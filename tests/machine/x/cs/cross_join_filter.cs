using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        List<int> nums = new List<int> { 1, 2, 3 };
        List<string> letters = new List<string> { "A", "B" };
        var pairs = new Func<List<dynamic>>(() =>
        {
            var _res = new List<dynamic>();
            foreach (var n in nums)
            {
                if (!(((n % 2) == 0))) continue;
                foreach (var l in letters)
                {
                    if (((n % 2) == 0))
                    {
                        _res.Add(new Dictionary<dynamic, dynamic> { { "n", n }, { "l", l } });
                    }
                }
            }
            return _res;
        })();
        Console.WriteLine("--- Even pairs ---");
        foreach (var p in pairs)
        {
            Console.WriteLine(string.Join(" ", new[] { Convert.ToString(p.n), Convert.ToString(p.l) }));
        }
    }
}
