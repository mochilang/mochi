import java.io.*;
import java.util.*;

class Main {
    static List<String> findItinerary(List<String[]> tickets) {
        Map<String, ArrayList<String>> graph = new HashMap<>();
        for (String[] t : tickets) graph.computeIfAbsent(t[0], k -> new ArrayList<>()).add(t[1]);
        for (ArrayList<String> v : graph.values()) v.sort(Collections.reverseOrder());
        ArrayList<String> route = new ArrayList<>();
        visit("JFK", graph, route);
        Collections.reverse(route);
        return route;
    }
    static void visit(String a, Map<String, ArrayList<String>> graph, ArrayList<String> route) {
        ArrayList<String> dests = graph.get(a);
        while (dests != null && !dests.isEmpty()) {
            String next = dests.remove(dests.size() - 1);
            visit(next, graph, route);
            dests = graph.get(a);
        }
        route.add(a);
    }
    static String fmt(List<String> r) { StringBuilder sb = new StringBuilder("["); for (int i = 0; i < r.size(); i++) { if (i > 0) sb.append(','); sb.append('"').append(r.get(i)).append('"'); } return sb.append(']').toString(); }
    public static void main(String[] args) throws Exception { FastScanner fs = new FastScanner(System.in); String st = fs.next(); if (st == null) return; int t = Integer.parseInt(st); StringBuilder out = new StringBuilder(); for (int tc = 0; tc < t; tc++) { int m = Integer.parseInt(fs.next()); List<String[]> tickets = new ArrayList<>(); for (int i = 0; i < m; i++) tickets.add(new String[]{fs.next(), fs.next()}); if (tc > 0) out.append("\n\n"); out.append(fmt(findItinerary(tickets))); } System.out.print(out); }
    static class FastScanner { private final String[] data; private int idx = 0; FastScanner(InputStream is) throws IOException { data = new String(is.readAllBytes()).trim().split("\\s+"); } String next() { return idx >= data.length || data.length == 1 && data[0].isEmpty() ? null : data[idx++]; } }
}
