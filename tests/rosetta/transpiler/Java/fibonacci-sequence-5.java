public class Main {
    static java.util.Map<Integer,Integer> memo;

    static int fib(int n) {
        if (((Boolean)(memo.containsKey(n)))) {
            return ((int)(memo).getOrDefault(n, 0));
        }
        int v = fib(n - 1) + fib(n - 2);
memo.put(n, v);
        return v;
    }

    static void main() {
        int i = 1;
        while (i <= 30) {
            System.out.println(_p(fib(i)));
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            memo = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>(java.util.Map.ofEntries(java.util.Map.entry(0, 0), java.util.Map.entry(1, 1)))));
            main();
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
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
