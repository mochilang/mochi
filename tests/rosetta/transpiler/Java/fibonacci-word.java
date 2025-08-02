public class Main {

    static double entropy(String s) {
        java.util.Map<String,Integer> counts = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            if (((Boolean)(counts.containsKey(ch)))) {
counts.put(ch, (int)(((int)(counts).getOrDefault(ch, 0))) + 1);
            } else {
counts.put(ch, 1);
            }
            i = i + 1;
        }
        double hm = 0.0;
        for (var k : new java.util.ArrayList<>(counts.keySet())) {
            double c = (double)(((double)(counts).getOrDefault(k, 0)));
            hm = hm + c * (Math.log(c) / Math.log(2.0));
        }
        double l = ((Number)(_runeLen(s))).doubleValue();
        return (Math.log(l) / Math.log(2.0)) - hm / l;
    }

    static String fibonacciWord(int n) {
        String a = "1";
        String b = "0";
        int i_1 = 1;
        while (i_1 < n) {
            String tmp = b;
            b = b + a;
            a = tmp;
            i_1 = i_1 + 1;
        }
        return a;
    }

    static void main() {
        System.out.println(String.valueOf(String.valueOf(pad("N", 3)) + String.valueOf(pad("Length", 9))) + "  Entropy      Word");
        int n = 1;
        while (n < 10) {
            String s = String.valueOf(fibonacciWord(n));
            System.out.println(String.valueOf(String.valueOf(pad(_p(n), 3)) + String.valueOf(pad(_p(_runeLen(s)), 9))) + "  " + String.valueOf(fmt(entropy(s))) + "  " + s);
            n = n + 1;
        }
        while (n <= 37) {
            String s_1 = String.valueOf(fibonacciWord(n));
            System.out.println(String.valueOf(String.valueOf(pad(_p(n), 3)) + String.valueOf(pad(_p(_runeLen(s_1)), 9))) + "  " + String.valueOf(fmt(entropy(s_1))));
            n = n + 1;
        }
    }

    static String pad(String s, int w) {
        String t = s;
        while (_runeLen(t) < w) {
            t = " " + t;
        }
        return t;
    }

    static String fmt(double x) {
        double y = floorf(x * 100000000.0 + 0.5) / 100000000.0;
        String s_2 = _p(y);
        int dot = ((Number)(s_2.indexOf("."))).intValue();
        if (dot == 0 - 1) {
            s_2 = s_2 + ".00000000";
        } else {
            int d = _runeLen(s_2) - dot - 1;
            while (d < 8) {
                s_2 = s_2 + "0";
                d = d + 1;
            }
        }
        return s_2;
    }

    static double floorf(double x) {
        int y_1 = ((Number)(x)).intValue();
        return ((Number)(y_1)).doubleValue();
    }

    static int indexOf(String s, String ch) {
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            if ((_substr(s, i_2, i_2 + 1).equals(ch))) {
                return i_2;
            }
            i_2 = i_2 + 1;
        }
        return 0 - 1;
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
