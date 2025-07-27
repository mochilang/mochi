public class Main {
    static class cds {
        int i;
        String s;
        int[] b;
        java.util.Map<Integer,Boolean> m;
        cds(int i, String s, int[] b, java.util.Map<Integer,Boolean> m) {
            this.i = i;
            this.s = s;
            this.b = b;
            this.m = m;
        }
        @Override public String toString() {
            return String.format("{'i': %s, 's': '%s', 'b': %s, 'm': %s}", String.valueOf(i), String.valueOf(s), String.valueOf(b), String.valueOf(m));
        }
    }

    static cds c1 = new cds(1, "one", new int[]{117, 110, 105, 116}, new java.util.LinkedHashMap<Integer, Boolean>(java.util.Map.ofEntries(java.util.Map.entry(1, true))));
    static cds c2 = deepcopy(c1);

    static int[] copyList(int[] src) {
        int[] out = new int[]{};
        for (int v : src) {
            out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(v)).toArray();
        }
        return out;
    }

    static java.util.Map<Integer,Boolean> copyMap(java.util.Map<Integer,Boolean> src) {
        java.util.Map<Integer,Boolean> out = new java.util.LinkedHashMap<Integer, Boolean>();
        for (int k : src.keySet()) {
out.put(k, ((boolean)src.getOrDefault(k, false)));
        }
        return out;
    }

    static cds deepcopy(cds c) {
        return new cds(c.i, c.s, copyList(c.b), copyMap(c.m));
    }

    static String cdsStr(cds c) {
        String bs = "[";
        int i = 0;
        while (i < c.b.length) {
            bs = bs + String.valueOf(c.b[i]);
            if (i < c.b.length - 1) {
                bs = bs + " ";
            }
            i = i + 1;
        }
        bs = bs + "]";
        String ms = "map[";
        boolean first = true;
        for (var k : c.m) {
            if (!first) {
                ms = ms + " ";
            }
            ms = ms + String.valueOf(k) + ":" + String.valueOf(((boolean)c.m.getOrDefault(k, false)));
            first = false;
        }
        ms = ms + "]";
        return "{" + String.valueOf(c.i) + " " + c.s + " " + bs + " " + ms + "}";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(cdsStr(c1));
            System.out.println(cdsStr(c2));
            c1 = new cds(0, "nil", new int[]{122, 101, 114, 111}, new java.util.LinkedHashMap<Integer, Boolean>(java.util.Map.ofEntries(java.util.Map.entry(1, false))));
            System.out.println(cdsStr(c1));
            System.out.println(cdsStr(c2));
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
}
