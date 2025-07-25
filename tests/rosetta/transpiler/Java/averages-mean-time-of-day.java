public class Main {
    static double PI = 3.141592653589793;

    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 8) {
            double denom = ((Number)(((2 * n) * (2 * n + 1)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double cosApprox(double x) {
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n <= 8) {
            double denom = ((Number)(((2 * n - 1) * (2 * n)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double atanApprox(double x) {
        if (x > 1.0) {
            return PI / 2.0 - x / (x * x + 0.28);
        }
        if (x < (-1.0)) {
            return -PI / 2.0 - x / (x * x + 0.28);
        }
        return x / (1.0 + 0.28 * x * x);
    }

    static double atan2Approx(double y, double x) {
        if (x > 0.0) {
            return atanApprox(y / x);
        }
        if (x < 0.0) {
            if (y >= 0.0) {
                return atanApprox(y / x) + PI;
            }
            return atanApprox(y / x) - PI;
        }
        if (y > 0.0) {
            return PI / 2.0;
        }
        if (y < 0.0) {
            return -PI / 2.0;
        }
        return 0.0;
    }

    static int digit(String ch) {
        String digits = "0123456789";
        int i = 0;
        while (i < digits.length()) {
            if ((digits.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return 0;
    }

    static int parseTwo(String s, int idx) {
        return digit(s.substring(idx, idx + 1)) * 10 + digit(s.substring(idx + 1, idx + 2));
    }

    static double parseSec(String s) {
        int h = parseTwo(s, 0);
        int m = parseTwo(s, 3);
        int sec = parseTwo(s, 6);
        int tmp = (h * 60 + m) * 60 + sec;
        return ((Number)(tmp)).doubleValue();
    }

    static String pad(int n) {
        if (n < 10) {
            return "0" + String.valueOf(n);
        }
        return String.valueOf(n);
    }

    static String meanTime(String[] times) {
        double ssum = 0.0;
        double csum = 0.0;
        int i = 0;
        while (i < times.length) {
            double sec = parseSec(times[i]);
            double ang = sec * 2.0 * PI / 86400.0;
            ssum = ssum + sinApprox(ang);
            csum = csum + cosApprox(ang);
            i = i + 1;
        }
        double theta = atan2Approx(ssum, csum);
        double frac = theta / (2.0 * PI);
        while (frac < 0.0) {
            frac = frac + 1.0;
        }
        double total = frac * 86400.0;
        int si = ((Number)(total)).intValue();
        int h = ((Number)((si / 3600))).intValue();
        int m = ((Number)((((Number)((Math.floorMod(si, 3600)))).intValue() / 60))).intValue();
        int s = ((Number)((Math.floorMod(si, 60)))).intValue();
        return String.valueOf(String.valueOf(pad(h)) + ":" + pad(m)) + ":" + pad(s);
    }

    static void main() {
        String[] inputs = new String[]{"23:00:17", "23:40:20", "00:12:45", "00:17:19"};
        System.out.println(meanTime(inputs));
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
