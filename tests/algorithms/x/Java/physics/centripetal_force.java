public class Main {

    static double centripetal(double mass, double velocity, double radius) {
        if ((double)(mass) < 0.0) {
            throw new RuntimeException(String.valueOf("The mass of the body cannot be negative"));
        }
        if ((double)(radius) <= 0.0) {
            throw new RuntimeException(String.valueOf("The radius is always a positive non zero integer"));
        }
        return (double)(mass) * (double)(velocity) * (double)(velocity) / (double)(radius);
    }

    static double floor(double x) {
        long i = (long)(((Number)(x)).intValue());
        if ((((Number)(i)).doubleValue()) > (double)(x)) {
            i = (long)((long)(i) - (long)(1));
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(long n) {
        double p = 1.0;
        long i_2 = 0L;
        while ((long)(i_2) < n) {
            p = p * 10.0;
            i_2 = (long)((long)(i_2) + (long)(1));
        }
        return p;
    }

    static double round(double x, long n) {
        double m = (double)(pow10(n));
        return (double)(floor((double)(x) * (double)(m) + 0.5)) / (double)(m);
    }

    static void show(double mass, double velocity, double radius) {
        double f = (double)(centripetal((double)(mass), (double)(velocity), (double)(radius)));
        System.out.println(_p(round((double)(f), 2L)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            show(15.5, -30.0, 10.0);
            show(10.0, 15.0, 5.0);
            show(20.0, -50.0, 15.0);
            show(12.25, 40.0, 25.0);
            show(50.0, 100.0, 50.0);
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
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
