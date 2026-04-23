using System;
using System.Collections.Generic;

public class Program
{
    static void Visit(string airport, Dictionary<string, List<string>> graph, List<string> route)
    {
        List<string> dests;
        while (graph.TryGetValue(airport, out dests) && dests.Count > 0)
        {
            int last = dests.Count - 1;
            string next = dests[last];
            dests.RemoveAt(last);
            Visit(next, graph, route);
        }
        route.Add(airport);
    }
    static List<string> FindItinerary(List<Tuple<string,string>> tickets)
    {
        var graph = new Dictionary<string, List<string>>();
        foreach (var t in tickets) { if (!graph.ContainsKey(t.Item1)) graph[t.Item1] = new List<string>(); graph[t.Item1].Add(t.Item2); }
        foreach (var list in graph.Values) { list.Sort(); list.Reverse(); }
        var route = new List<string>(); Visit("JFK", graph, route); route.Reverse(); return route;
    }
    static string Fmt(List<string> r) => "[" + string.Join(",", r.ConvertAll(s => "\"" + s + "\"")) + "]";
    public static void Main() { string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries); if (data.Length == 0) return; int idx = 0, t = int.Parse(data[idx++]); var outv = new List<string>(); for (int tc = 0; tc < t; tc++) { int m = int.Parse(data[idx++]); var tickets = new List<Tuple<string,string>>(); for (int i = 0; i < m; i++) tickets.Add(Tuple.Create(data[idx++], data[idx++])); outv.Add(Fmt(FindItinerary(tickets))); } Console.Write(string.Join("\n\n", outv)); }
}
