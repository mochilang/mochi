using System;
using System.Collections.Generic;

class Program {
    class Row {
        public string Dept = "";
        public string Name = "";
        public int Salary;
    }

    static string SolveCase(List<Tuple<int, string>> departments, List<Tuple<int, string, int, int>> employees) {
        var deptName = new Dictionary<int, string>();
        foreach (var d in departments) deptName[d.Item1] = d.Item2;
        var groups = new Dictionary<int, List<Tuple<string, int>>>();
        foreach (var e in employees) {
            if (!groups.ContainsKey(e.Item4)) groups[e.Item4] = new List<Tuple<string, int>>();
            groups[e.Item4].Add(Tuple.Create(e.Item2, e.Item3));
        }
        var rows = new List<Row>();
        foreach (var entry in groups) {
            var salaries = new List<int>(new HashSet<int>(entry.Value.ConvertAll(x => x.Item2)));
            salaries.Sort((a, b) => b.CompareTo(a));
            var keep = new HashSet<int>();
            for (int i = 0; i < salaries.Count && i < 3; i++) keep.Add(salaries[i]);
            foreach (var item in entry.Value) if (keep.Contains(item.Item2)) rows.Add(new Row { Dept = deptName[entry.Key], Name = item.Item1, Salary = item.Item2 });
        }
        rows.Sort((a, b) => a.Dept != b.Dept ? string.CompareOrdinal(a.Dept, b.Dept) : a.Salary != b.Salary ? b.Salary.CompareTo(a.Salary) : string.CompareOrdinal(a.Name, b.Name));
        var lines = new List<string> { rows.Count.ToString() };
        foreach (var row in rows) lines.Add($"{row.Dept},{row.Name},{row.Salary}");
        return string.Join("\n", lines);
    }

    static void Main() {
        var toks = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (toks.Length == 0) return;
        int idx = 0, t = int.Parse(toks[idx++]);
        var outCases = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int d = int.Parse(toks[idx++]), e = int.Parse(toks[idx++]);
            var departments = new List<Tuple<int, string>>();
            for (int i = 0; i < d; i++) departments.Add(Tuple.Create(int.Parse(toks[idx++]), toks[idx++]));
            var employees = new List<Tuple<int, string, int, int>>();
            for (int i = 0; i < e; i++) employees.Add(Tuple.Create(int.Parse(toks[idx++]), toks[idx++], int.Parse(toks[idx++]), int.Parse(toks[idx++])));
            outCases.Add(SolveCase(departments, employees));
        }
        Console.Write(string.Join("\n\n", outCases));
    }
}
