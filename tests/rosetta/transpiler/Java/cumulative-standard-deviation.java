public class Main {
    static class Rsdv {
        double n;
        double a;
        double q;
        Rsdv(double n, double a, double q) {
            this.n = n;
            this.a = a;
            this.q = q;
        }
        @Override public String toString() {
            return String.format("{'n': %s, 'a': %s, 'q': %s}", String.valueOf(n), String.valueOf(a), String.valueOf(q));
        }
    }


    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double g = x;
        int i = 0;
        while (i < 20) {
            g = (g + x / g) / 2.0;
            i = i + 1;
        }
        return g;
    }

    static Rsdv newRsdv() {
        return new Rsdv(0.0, 0.0, 0.0);
    }

    static Rsdv add(Rsdv r, double x) {
        double n1 = r.n + 1.0;
        double a1 = r.a + (x - r.a) / n1;
        double q1 = r.q + (x - r.a) * (x - a1);
        return new Rsdv(n1, a1, q1);
    }

    static double sd(Rsdv r) {
        return sqrtApprox(r.q / r.n);
    }

    static void main() {
        Rsdv r = newRsdv();
        for (double x : new double[]{2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0}) {
            r = add(r, x);
            System.out.println(String.valueOf(sd(r)));
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
