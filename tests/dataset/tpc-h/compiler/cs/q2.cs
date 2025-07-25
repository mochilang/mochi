// Generated by Mochi compiler v0.10.27 on 2025-07-17T18:30:15Z
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;

class Program
{
    static void test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part(dynamic result)
    {
        expect(_equal(result, new List<dynamic> { new Dictionary<string, dynamic> { { "s_acctbal", 1000.000000 }, { "s_name", "BestSupplier" }, { "n_name", "FRANCE" }, { "p_partkey", 1000 }, { "p_mfgr", "M1" }, { "s_address", "123 Rue" }, { "s_phone", "123" }, { "s_comment", "Fast and reliable" }, { "ps_supplycost", 10.000000 } } }));
    }

    static void Main()
    {
        var region = new List<dynamic> { new Dictionary<string, dynamic> { { "r_regionkey", 1 }, { "r_name", "EUROPE" } }, new Dictionary<string, dynamic> { { "r_regionkey", 2 }, { "r_name", "ASIA" } } };
        var nation = new List<dynamic> { new Dictionary<string, dynamic> { { "n_nationkey", 10 }, { "n_regionkey", 1 }, { "n_name", "FRANCE" } }, new Dictionary<string, dynamic> { { "n_nationkey", 20 }, { "n_regionkey", 2 }, { "n_name", "CHINA" } } };
        var supplier = new List<dynamic> { new Dictionary<string, dynamic> { { "s_suppkey", 100 }, { "s_name", "BestSupplier" }, { "s_address", "123 Rue" }, { "s_nationkey", 10 }, { "s_phone", "123" }, { "s_acctbal", 1000.000000 }, { "s_comment", "Fast and reliable" } }, new Dictionary<string, dynamic> { { "s_suppkey", 200 }, { "s_name", "AltSupplier" }, { "s_address", "456 Way" }, { "s_nationkey", 20 }, { "s_phone", "456" }, { "s_acctbal", 500.000000 }, { "s_comment", "Slow" } } };
        var part = new List<dynamic> { new Dictionary<string, dynamic> { { "p_partkey", 1000 }, { "p_type", "LARGE BRASS" }, { "p_size", 15 }, { "p_mfgr", "M1" } }, new Dictionary<string, dynamic> { { "p_partkey", 2000 }, { "p_type", "SMALL COPPER" }, { "p_size", 15 }, { "p_mfgr", "M2" } } };
        var partsupp = new List<dynamic> { new Dictionary<string, dynamic> { { "ps_partkey", 1000 }, { "ps_suppkey", 100 }, { "ps_supplycost", 10.000000 } }, new Dictionary<string, dynamic> { { "ps_partkey", 1000 }, { "ps_suppkey", 200 }, { "ps_supplycost", 15.000000 } } };
        var europe_nations = region.Join(nation, r => n["n_regionkey"], n => r["r_regionkey"], (r, n) => n).Where(r => (r["r_name"] == "EUROPE")).ToList();
        var europe_suppliers = supplier.Join(europe_nations, s => s["s_nationkey"], n => n["n_nationkey"], (s, n) => new Dictionary<string, Dictionary<string, dynamic>> { { "s", s }, { "n", n } }).ToList();
        var target_parts = part.Where(p => (p["p_size"] == 15) && (p["p_type"] == "LARGE BRASS")).Select(p => p).ToList();
        var target_partsupp = (
    from ps in partsupp
    join p in target_parts on ps["ps_partkey"] equals p["p_partkey"]
    join s in europe_suppliers on ps["ps_suppkey"] equals s["s"].s_suppkey
    select new Dictionary<string, dynamic> { { "s_acctbal", s["s"].s_acctbal }, { "s_name", s["s"].s_name }, { "n_name", s["n"].n_name }, { "p_partkey", p["p_partkey"] }, { "p_mfgr", p["p_mfgr"] }, { "s_address", s["s"].s_address }, { "s_phone", s["s"].s_phone }, { "s_comment", s["s"].s_comment }, { "ps_supplycost", ps["ps_supplycost"] } }
).ToList();
        var costs = target_partsupp.Select(x => x["ps_supplycost"]).ToList();
        var min_cost = Enumerable.Min(costs);
        var result = target_partsupp.Where(x => (x["ps_supplycost"] == min_cost)).OrderBy(x => (-x["s_acctbal"])).Select(x => x).ToList();
        Console.WriteLine(JsonSerializer.Serialize(result));
        test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part(result);
    }
    static void expect(bool cond)
    {
        if (!cond) throw new Exception("expect failed");
    }

    static bool _equal(dynamic a, dynamic b)
    {
        if (a is System.Collections.IEnumerable ae && b is System.Collections.IEnumerable be && a is not string && b is not string)
        {
            var ea = ae.GetEnumerator();
            var eb = be.GetEnumerator();
            while (true)
            {
                bool ha = ea.MoveNext();
                bool hb = eb.MoveNext();
                if (ha != hb) return false;
                if (!ha) break;
                if (!_equal(ea.Current, eb.Current)) return false;
            }
            return true;
        }
        if ((a is int || a is long || a is float || a is double) && (b is int || b is long || b is float || b is double))
        {
            return Convert.ToDouble(a) == Convert.ToDouble(b);
        }
        if (a != null && b != null && a.GetType() != b.GetType())
        {
            return JsonSerializer.Serialize(a) == JsonSerializer.Serialize(b);
        }
        if (a != null && b != null && !a.GetType().IsPrimitive && !b.GetType().IsPrimitive && a is not string && b is not string)
        {
            return JsonSerializer.Serialize(a) == JsonSerializer.Serialize(b);
        }
        return Equals(a, b);
    }

}
