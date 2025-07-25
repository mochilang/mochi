public class Main {

    public static void main(String[] args) {
        {
            int _benchStart = _now();
            int _benchMem = _mem();
            int n = 1000;
            int s = 0;
            for (int i = 1; i < n; i++) {
                s = s + i;
            }
            int _benchDuration = (_now() - _benchStart) / 1000;
            int _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"simple\"");
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
        return (int)System.currentTimeMillis();
    }

    static int _mem() {
        Runtime rt = Runtime.getRuntime();
        return (int)(rt.totalMemory() - rt.freeMemory());
    }
}
