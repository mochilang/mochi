public class Main {
    static class Node {
        String name;
        String[] inbound;
        String[] outbound;
        Node(String name, String[] inbound, String[] outbound) {
            this.name = name;
            this.inbound = inbound;
            this.outbound = outbound;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'inbound': %s, 'outbound': %s}", String.valueOf(name), String.valueOf(inbound), String.valueOf(outbound));
        }
    }

    static String[] names;
    static int[][] graph;
    static Node[] nodes = new Node[0];
    static int ri = 0;

    static String node_to_string(Node n) {
        return "<node=" + n.name + " inbound=" + String.valueOf(n.inbound) + " outbound=" + String.valueOf(n.outbound) + ">";
    }

    static java.util.Map<String,Double> page_rank(Node[] nodes, int limit, double d) {
        java.util.Map<String,Double> ranks = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        for (Node n : nodes) {
ranks.put(n.name, 1.0);
        }
        java.util.Map<String,Double> outbounds = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        for (Node n : nodes) {
outbounds.put(n.name, 1.0 * n.outbound.length);
        }
        int i = 0;
        while (i < limit) {
            System.out.println("======= Iteration " + _p(i + 1) + " =======");
            for (Node n : nodes) {
                double sum_val = 0.0;
                for (String ib : n.inbound) {
                    sum_val = sum_val + (double)(((double)(ranks).getOrDefault(ib, 0.0))) / (double)(((double)(outbounds).getOrDefault(ib, 0.0)));
                }
ranks.put(n.name, (1.0 - d) + d * sum_val);
            }
            System.out.println(ranks);
            i = i + 1;
        }
        return ranks;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            names = ((String[])(new String[]{"A", "B", "C"}));
            graph = ((int[][])(new int[][]{new int[]{0, 1, 1}, new int[]{0, 0, 1}, new int[]{1, 0, 0}}));
            nodes = ((Node[])(new Node[]{}));
            for (String name : names) {
                nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new Node(name, new String[]{}, new String[]{}))).toArray(Node[]::new)));
            }
            ri = 0;
            while (ri < graph.length) {
                int[] row = ((int[])(graph[ri]));
                int ci = 0;
                while (ci < row.length) {
                    if (row[ci] == 1) {
                        Node n_in = nodes[ci];
n_in.inbound = java.util.stream.Stream.concat(java.util.Arrays.stream(n_in.inbound), java.util.stream.Stream.of(names[ri])).toArray(String[]::new);
nodes[ci] = n_in;
                        Node n_out = nodes[ri];
n_out.outbound = java.util.stream.Stream.concat(java.util.Arrays.stream(n_out.outbound), java.util.stream.Stream.of(names[ci])).toArray(String[]::new);
nodes[ri] = n_out;
                    }
                    ci = ci + 1;
                }
                ri = ri + 1;
            }
            System.out.println("======= Nodes =======");
            for (Node n : nodes) {
                System.out.println(n);
            }
            page_rank(((Node[])(nodes)), 3, 0.85);
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
