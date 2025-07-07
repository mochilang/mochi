using System;
using System.Collections.Generic;
using System.Linq;

class Item
{
    public string cat;
    public int val;
    public bool flag;
}

class Program
{
    static void Main()
    {
        var items = new List<Item>
        {
            new Item
            {
                cat = "a",
                val = 10,
                flag = true,
            },
            new Item
            {
                cat = "a",
                val = 5,
                flag = false,
            },
            new Item
            {
                cat = "b",
                val = 20,
                flag = true,
            },
        };

        var result = items
            .GroupBy(i => i.cat)
            .OrderBy(g => g.Key)
            .Select(g => new
            {
                cat = g.Key,
                share = g.Where(x => x.flag).Sum(x => x.val) / (double)g.Sum(x => x.val),
            });

        foreach (var r in result)
        {
            Console.WriteLine($"{r.cat}: share {r.share}");
        }
    }
}
