public class Main {
    static int max;
    static java.util.Map<Integer,String> words;
    static int[] keys;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            max = 20;
            words = ((java.util.Map<Integer,String>)(new java.util.LinkedHashMap<Integer, String>(java.util.Map.ofEntries(java.util.Map.entry(3, "Fizz"), java.util.Map.entry(5, "Buzz"), java.util.Map.entry(7, "Baxx")))));
            keys = ((int[])(new int[]{3, 5, 7}));
            for (int i = 1; i < (max + 1); i++) {
                String text = "";
                for (int n : keys) {
                    if (Math.floorMod(i, n) == 0) {
                        text = text + ((String)(words).get(n));
                    }
                }
                if ((text.equals(""))) {
                    text = _p(i);
                }
                System.out.println(text);
            }
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
