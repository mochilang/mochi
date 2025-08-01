// Generated by Mochi 0.10.33 on 2025-07-21 18:06 +0700
using System;
using System.Collections.Generic;
using System.Linq;
struct Nation {
    public int id;
    public string name;
    public override string ToString() => $"{{id: {id}, name: {name}}}";
}
struct Supplier {
    public int id;
    public int nation;
    public override string ToString() => $"{{id: {id}, nation: {nation}}}";
}
struct Partsupp {
    public int part;
    public int supplier;
    public double cost;
    public int qty;
    public override string ToString() => $"{{part: {part}, supplier: {supplier}, cost: {cost}, qty: {qty}}}";
}
struct PsResult {
    public int part;
    public double value;
    public override string ToString() => $"{{part: {part}, value: {value}}}";
}
struct GGroup {
    public int key;
    public PsResult[] items;
    public override string ToString() => $"{{key: {key}, items: {items}}}";
}
struct GResult {
    public int part;
    public double total;
    public override string ToString() => $"{{part: {part}, total: {total}}}";
}
class Program {
    static Nation[] nations = new Nation[]{new Nation{id = 1, name = "A"}, new Nation{id = 2, name = "B"}};
    static Supplier[] suppliers = new Supplier[]{new Supplier{id = 1, nation = 1}, new Supplier{id = 2, nation = 2}};
    static Partsupp[] partsupp = new Partsupp[]{new Partsupp{part = 100, supplier = 1, cost = 10, qty = 2}, new Partsupp{part = 100, supplier = 2, cost = 20, qty = 1}, new Partsupp{part = 200, supplier = 1, cost = 5, qty = 3}};
    static PsResult[] filtered = (from ps in partsupp ssuppliersps.suppliers.id nnationss.nationn.id where (n.name == "A") select new PsResult{part = ps.part, value = (ps.cost * ps.qty)}).ToArray();
    static GResult[] grouped = (from x in filtered group x by x.part into gTmp let g = new GGroup{key = gTmp.Key, items = gTmp.ToArray()} select new GResult{part = g.key, total = ((from r in g.items select r.value).ToArray().Sum())}).ToArray();
    static void Main() {
        Console.WriteLine(string.Join(" ", grouped).TrimEnd());
    }
}
