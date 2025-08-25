public class Main {

    static double centripetal(double mass, double velocity, double radius) {
        if ((double)(mass) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("The mass of the body cannot be negative"));
        }
        if ((double)(radius) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("The radius is always a positive non zero integer"));
        }
        return (double)((double)((double)((double)(mass) * (double)(velocity)) * (double)(velocity)) / (double)(radius));
    }

    static double floor(double x) {
        java.math.BigInteger i = new java.math.BigInteger(String.valueOf(((Number)(x)).intValue()));
        if ((double)((((Number)(i)).doubleValue())) > (double)(x)) {
            i = new java.math.BigInteger(String.valueOf(i.subtract(java.math.BigInteger.valueOf(1))));
        }
        return (double)(((Number)(i)).doubleValue());
    }

    static double pow10(java.math.BigInteger n) {
        double p = (double)(1.0);
        java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
        while (i_2.compareTo(n) < 0) {
            p = (double)((double)(p) * (double)(10.0));
            i_2 = new java.math.BigInteger(String.valueOf(i_2.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(p);
    }

    static double round(double x, java.math.BigInteger n) {
        double m = (double)(pow10(new java.math.BigInteger(String.valueOf(n))));
        return (double)(Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)) / (double)(m));
    }

    static void show(double mass, double velocity, double radius) {
        double f = (double)(centripetal((double)(mass), (double)(velocity), (double)(radius)));
        System.out.println(_p(round((double)(f), java.math.BigInteger.valueOf(2))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            show((double)(15.5), (double)(-30.0), (double)(10.0));
            show((double)(10.0), (double)(15.0), (double)(5.0));
            show((double)(20.0), (double)(-50.0), (double)(15.0));
            show((double)(12.25), (double)(40.0), (double)(25.0));
            show((double)(50.0), (double)(100.0), (double)(50.0));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
