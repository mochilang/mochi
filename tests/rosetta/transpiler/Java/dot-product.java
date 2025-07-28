public class Main {
    static class DotResult {
        int value;
        boolean ok;
        DotResult(int value, boolean ok) {
            this.value = value;
            this.ok = ok;
        }
        @Override public String toString() {
            return String.format("{'value': %s, 'ok': %s}", String.valueOf(value), String.valueOf(ok));
        }
    }


    static DotResult dot(int[] x, int[] y) {
        if (x.length != y.length) {
            return new DotResult(0, false);
        }
        int sum = 0;
        int i = 0;
        while (i < x.length) {
            sum = sum + x[i] * y[i];
            i = i + 1;
        }
        return new DotResult(sum, true);
    }

    static void main() {
        DotResult r = dot(new int[]{1, 3, -5}, new int[]{4, -2, -1});
        if (!r.ok) {
            System.out.println("incompatible lengths");
        } else {
            System.out.println(String.valueOf(r.value));
        }
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
