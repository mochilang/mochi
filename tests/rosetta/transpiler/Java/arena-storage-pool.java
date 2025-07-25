public class Main {

    static int[] poolPut(int[] p, int x) {
        return java.util.stream.IntStream.concat(java.util.Arrays.stream(p), java.util.stream.IntStream.of(x)).toArray();
    }

    static java.util.Map<String,Object> poolGet(int[] p) {
        if (p.length == 0) {
            System.out.println("pool empty");
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("pool", p), java.util.Map.entry("val", 0)));
        }
        int idx = p.length - 1;
        int v = p[idx];
        p = java.util.Arrays.copyOfRange(p, 0, idx);
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("pool", p), java.util.Map.entry("val", v)));
    }

    static int[] clearPool(int[] p) {
        return new int[]{};
    }

    static void main() {
        int[] pool = new int[]{};
        int i = 1;
        int j = 2;
        System.out.println(String.valueOf(i + j));
        pool = poolPut(pool, i);
        pool = poolPut(pool, j);
        i = 0;
        j = 0;
        java.util.Map<String,Object> res1 = poolGet(pool);
        pool = (int[])(((int[])res1.get("pool")));
        i = (int)(((int)res1.get("val")));
        java.util.Map<String,Object> res2 = poolGet(pool);
        pool = (int[])(((int[])res2.get("pool")));
        j = (int)(((int)res2.get("val")));
        i = 4;
        j = 5;
        System.out.println(String.valueOf(i + j));
        pool = poolPut(pool, i);
        pool = poolPut(pool, j);
        i = 0;
        j = 0;
        pool = clearPool(pool);
        java.util.Map<String,Object> res3 = poolGet(pool);
        pool = (int[])(((int[])res3.get("pool")));
        i = (int)(((int)res3.get("val")));
        java.util.Map<String,Object> res4 = poolGet(pool);
        pool = (int[])(((int[])res4.get("pool")));
        j = (int)(((int)res4.get("val")));
        i = 7;
        j = 8;
        System.out.println(String.valueOf(i + j));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
}
