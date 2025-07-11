using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<People> people = new List<People> { new People { name = "Alice", age = 30, city = "Paris" }, new People { name = "Bob", age = 15, city = "Hanoi" }, new People { name = "Charlie", age = 65, city = "Paris" }, new People { name = "Diana", age = 45, city = "Hanoi" }, new People { name = "Eve", age = 70, city = "Paris" }, new People { name = "Frank", age = 22, city = "Hanoi" } };
        List<Stat> stats = _group_by<People, string>(people, person => person.city).Select(g => new Stat { city = g.Key, count = Enumerable.Count(g), avg_age = _avg(g.Items.Select(p => p.age).ToArray()) }).ToList();
        Console.WriteLine("--- People grouped by city ---");
        foreach (var s in stats) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(s.city), Convert.ToString(": count ="), Convert.ToString(s.count), Convert.ToString(", avg_age ="), Convert.ToString(s.avg_age) }));
        }
    }
    public class People {
        public string name;
        public int age;
        public string city;
    }
    
    
    public class Stat {
        public string city;
        public int count;
        public double avg_age;
    }
    
    
    
    static double _avg(dynamic v) {
        if (v == null) return 0.0;
        int _n = 0;
        double _sum = 0;
        foreach (var it in v) {
            _sum += Convert.ToDouble(it);
            _n++;
        }
        return _n == 0 ? 0.0 : _sum / _n;
    }
    
    static List<_Group<TKey, TItem>> _group_by<TItem, TKey>(IEnumerable<TItem> src, Func<TItem, TKey> keyfn) {
        var groups = new Dictionary<string, _Group<TKey, TItem>>();
        var order = new List<string>();
        foreach (var it in src) {
            var key = keyfn(it);
            var ks = Convert.ToString(key);
            if (!groups.TryGetValue(ks, out var g)) {
                g = new _Group<TKey, TItem>(key);
                groups[ks] = g;
                order.Add(ks);
            }
            g.Items.Add(it);
        }
        var res = new List<_Group<TKey, TItem>>();
        foreach (var k in order) res.Add(groups[k]);
        return res;
    }
    
}
