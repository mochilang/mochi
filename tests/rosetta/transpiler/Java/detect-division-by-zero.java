public class Main {
    static class DivResult {
        int q;
        boolean ok;
        DivResult(int q, boolean ok) {
            this.q = q;
            this.ok = ok;
        }
        @Override public String toString() {
            return String.format("{'q': %s, 'ok': %s}", String.valueOf(q), String.valueOf(ok));
        }
    }


    static DivResult divCheck(int x, int y) {
        if (y == 0) {
            return new DivResult(0, false);
        }
        return new DivResult(x / y, true);
    }

    static void printResult(DivResult r) {
        System.out.println(String.valueOf(r.q) + " " + String.valueOf(r.ok));
    }

    static void main() {
        printResult(divCheck(3, 2));
        printResult(divCheck(3, 0));
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
