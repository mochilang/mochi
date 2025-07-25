public class Main {

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double floorf(double x) {
        int y = ((Number)(x)).intValue();
        return ((Number)(y)).doubleValue();
    }

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String fmtF(double x) {
        double y = floorf(x * 10000.0 + 0.5) / 10000.0;
        String s = String.valueOf(y);
        int dot = indexOf(s, ".");
        if (dot == 0 - 1) {
            s = String.valueOf(s + ".0000");
        } else {
            int decs = s.length() - dot - 1;
            if (decs > 4) {
                s = s.substring(0, dot + 5);
            } else {
                while (decs < 4) {
                    s = String.valueOf(s + "0");
                    decs = decs + 1;
                }
            }
        }
        return s;
    }

    static String padInt(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = String.valueOf(" " + s);
        }
        return s;
    }

    static String padFloat(double x, int width) {
        String s = String.valueOf(fmtF(x));
        while (s.length() < width) {
            s = String.valueOf(" " + s);
        }
        return s;
    }

    static double avgLen(int n) {
        int tests = 10000;
        int sum = 0;
        int seed = 1;
        int t = 0;
        while (t < tests) {
            boolean[] visited = new boolean[]{};
            int i = 0;
            while (i < n) {
                visited = appendBool(visited, false);
                i = i + 1;
            }
            int x = 0;
            while (!(Boolean)visited[x]) {
visited[x] = true;
                sum = sum + 1;
                seed = Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
                x = Math.floorMod(seed, n);
            }
            t = t + 1;
        }
        return (((Number)(sum)).doubleValue()) / tests;
    }

    static double ana(int n) {
        double nn = ((Number)(n)).doubleValue();
        double term = 1.0;
        double sum = 1.0;
        double i = nn - 1.0;
        while (i >= 1.0) {
            term = term * (i / nn);
            sum = sum + term;
            i = i - 1.0;
        }
        return sum;
    }

    static void main() {
        int nmax = 20;
        System.out.println(" N    average    analytical    (error)");
        System.out.println("===  =========  ============  =========");
        int n = 1;
        while (n <= nmax) {
            double a = avgLen(n);
            double b = ana(n);
            double err = absf(a - b) / b * 100.0;
            String line = String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(padInt(n, 3)) + "  " + padFloat(a, 9)) + "  " + padFloat(b, 12)) + "  (" + padFloat(err, 6)) + "%)");
            System.out.println(line);
            n = n + 1;
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
