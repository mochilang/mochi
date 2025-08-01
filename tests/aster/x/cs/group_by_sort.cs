// Generated by Mochi 0.10.33 on 2025-07-21 18:06 +0700
using System;
using System.Collections.Generic;
using System.Linq;
struct Item {
    public string cat;
    public int val;
    public override string ToString() => $"{{cat: {cat}, val: {val}}}";
}
struct GGroup {
    public string key;
    public Item[] items;
    public override string ToString() => $"{{key: {key}, items: {items}}}";
}
struct GResult {
    public string cat;
    public double total;
    public override string ToString() => $"{{cat: {cat}, total: {total}}}";
}
class Program {
    static Item[] items = new Item[]{new Item{cat = "a", val = 3}, new Item{cat = "a", val = 1}, new Item{cat = "b", val = 5}, new Item{cat = "b", val = 2}};
    static GResult[] grouped = (from i in items group i by i.cat into gTmp let g = new GGroup{key = gTmp.Key, items = gTmp.ToArray()} ((from x in g.items select x.val).ToArray().Sum()) select new GResult{cat = g.key, total = ((from x in g.items select x.val).ToArray().Sum())}).ToArray();
    static void Main() {
        Console.WriteLine(string.Join(" ", grouped).TrimEnd());
    }
}
