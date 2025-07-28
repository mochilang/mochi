public class Main {

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String fmt1(double x) {
        double y = ((Number)(((Number)(((x * 10.0) + 0.5))).intValue())).doubleValue() / 10.0;
        String s = String.valueOf(y);
        int dot = ((Number)(s.indexOf("."))).intValue();
        if (dot < 0) {
            s = s + ".0";
        }
        return s;
    }

    static void printColumnMatrix(double[] xs) {
        if (xs.length == 0) {
            return;
        }
        System.out.println("⎡" + String.valueOf(fmt1(xs[0])) + "⎤");
        int i = 1;
        while (i < xs.length - 1) {
            System.out.println("⎢" + String.valueOf(fmt1(xs[i])) + "⎥");
            i = i + 1;
        }
        System.out.println("⎣ " + String.valueOf(fmt1(xs[xs.length - 1])) + "⎦");
    }

    static double[] deconv(double[] g, double[] f) {
        double[] h = new double[]{};
        int n = 0;
        int hn = g.length - f.length + 1;
        while (n < hn) {
            double v = g[n];
            int lower = 0;
            if (n >= f.length) {
                lower = n - f.length + 1;
            }
            int i = lower;
            while (i < n) {
                v = v - h[i] * f[n - i];
                i = i + 1;
            }
            v = v / f[0];
            h = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(h), java.util.stream.DoubleStream.of(v)).toArray();
            n = n + 1;
        }
        return h;
    }

    static void main() {
        double[] h = new double[]{-8.0, -9.0, -3.0, -1.0, -6.0, 7.0};
        double[] f = new double[]{-3.0, -6.0, -1.0, 8.0, -6.0, 3.0, -1.0, -9.0, -9.0, 3.0, -2.0, 5.0, 2.0, -2.0, -7.0, -1.0};
        double[] g = new double[]{24.0, 75.0, 71.0, -34.0, 3.0, 22.0, -45.0, 23.0, 245.0, 25.0, 52.0, 25.0, -67.0, -96.0, 96.0, 31.0, 55.0, 36.0, 29.0, -43.0, -7.0};
        System.out.println("deconv(g, f) =");
        printColumnMatrix(deconv(g, f));
        System.out.println("");
        System.out.println("deconv(g, h) =");
        printColumnMatrix(deconv(g, h));
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
}
