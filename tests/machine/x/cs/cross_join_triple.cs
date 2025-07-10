using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        List<long> nums = new List<long> { 1, 2 };
        List<string> letters = new List<string> { "A", "B" };
        List<bool> bools = new List<bool> { true, false };
        var combos = new Func<List<Dictionary<string, dynamic>>>(() =>
        {
            var _res = new List<Dictionary<string, dynamic>>();
            foreach (var n in nums)
            {
                foreach (var l in letters)
                {
                    foreach (var b in bools)
                    {
                        _res.Add(new Dictionary<string, dynamic> { { "n", n }, { "l", l }, { "b", b } });
                    }
                }
            }
            return _res;
        })();
        Console.WriteLine("--- Cross Join of three lists ---");
        foreach (var c in combos)
        {
            Console.WriteLine(string.Join(" ", new[] { Convert.ToString(c["n"]), Convert.ToString(c["l"]), Convert.ToString(c["b"]) }));
        }
    }
}
