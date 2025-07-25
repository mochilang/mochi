public class Main {

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

    static String fmt3(double x) {
        double y = ((Number)(((Number)(((x * 1000.0) + 0.5))).intValue())).doubleValue() / 1000.0;
        String s = String.valueOf(y);
        int dot = indexOf(s, ".");
        if (dot == 0 - 1) {
            s = String.valueOf(s + ".000");
        } else {
            int decs = s.length() - dot - 1;
            if (decs > 3) {
                s = s.substring(0, dot + 4);
            } else {
                while (decs < 3) {
                    s = String.valueOf(s + "0");
                    decs = decs + 1;
                }
            }
        }
        return s;
    }

    static String pad(String s, int width) {
        String out = s;
        while (out.length() < width) {
            out = String.valueOf(" " + out);
        }
        return out;
    }

    static double[] smaSeries(double[] xs, int period) {
        double[] res = new double[]{};
        double sum = 0.0;
        int i = 0;
        while (i < xs.length) {
            sum = sum + xs[i];
            if (i >= period) {
                sum = sum - xs[i - period];
            }
            int denom = i + 1;
            if (denom > period) {
                denom = period;
            }
            res = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(sum / (((Number)(denom)).doubleValue()))).toArray();
            i = i + 1;
        }
        return res;
    }

    static void main() {
        double[] xs = new double[]{1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0};
        double[] sma3 = smaSeries(xs, 3);
        double[] sma5 = smaSeries(xs, 5);
        System.out.println("x       sma3   sma5");
        int i = 0;
        while (i < xs.length) {
            String line = String.valueOf(String.valueOf(String.valueOf(pad(String.valueOf(fmt3(xs[i])), 5)) + "  " + pad(String.valueOf(fmt3(sma3[i])), 5)) + "  " + pad(String.valueOf(fmt3(sma5[i])), 5));
            System.out.println(line);
            i = i + 1;
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
