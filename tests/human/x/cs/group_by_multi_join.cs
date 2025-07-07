using System;
using System.Collections.Generic;
using System.Linq;

class Nation { public int id; public string name; }
class Supplier { public int id; public int nation; }
class PartSupp { public int part; public int supplier; public double cost; public int qty; }

class Program {
    static void Main() {
        var nations = new List<Nation> {
            new Nation{ id=1, name="A" },
            new Nation{ id=2, name="B" }
        };
        var suppliers = new List<Supplier> {
            new Supplier{ id=1, nation=1 },
            new Supplier{ id=2, nation=2 }
        };
        var partsupp = new List<PartSupp> {
            new PartSupp{ part=100, supplier=1, cost=10.0, qty=2 },
            new PartSupp{ part=100, supplier=2, cost=20.0, qty=1 },
            new PartSupp{ part=200, supplier=1, cost=5.0, qty=3 }
        };

        var filtered = from ps in partsupp
                       join s in suppliers on ps.supplier equals s.id
                       join n in nations on s.nation equals n.id
                       where n.name == "A"
                       select new { part = ps.part, value = ps.cost * ps.qty };

        var grouped = from x in filtered
                      group x by x.part into g
                      select new { part = g.Key, total = g.Sum(r => r.value) };

        foreach (var g in grouped) {
            Console.WriteLine($"{g.part}: total {g.total}");
        }
    }
}
