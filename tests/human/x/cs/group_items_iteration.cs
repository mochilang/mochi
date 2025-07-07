using System;
using System.Collections.Generic;
using System.Linq;

class Data
{
    public string tag;
    public int val;
}

class Program
{
    static void Main()
    {
        var data = new List<Data>
        {
            new Data { tag = "a", val = 1 },
            new Data { tag = "a", val = 2 },
            new Data { tag = "b", val = 3 },
        };

        var groups = data.GroupBy(d => d.tag).Select(g => g);
        var tmp = new List<dynamic>();
        foreach (var g in groups)
        {
            int total = 0;
            foreach (var x in g)
            {
                total += x.val;
            }
            tmp.Add(new { tag = g.Key, total = total });
        }
        var result = tmp.OrderBy(r => r.tag);
        foreach (var r in result)
        {
            Console.WriteLine($"{r.tag}: {r.total}");
        }
    }
}
