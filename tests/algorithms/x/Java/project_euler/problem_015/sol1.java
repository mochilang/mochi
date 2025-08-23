public class Main {

    static java.math.BigInteger factorial(java.math.BigInteger n) {
        java.math.BigInteger result = java.math.BigInteger.valueOf(1);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(2);
        while (i_1.compareTo(n) <= 0) {
            result = result.multiply(i_1);
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return result;
    }

    static java.math.BigInteger solution(java.math.BigInteger n) {
        java.math.BigInteger total = java.math.BigInteger.valueOf(2).multiply(n);
        java.math.BigInteger k_1 = total.divide(java.math.BigInteger.valueOf(2));
        return factorial(total).divide((factorial(k_1).multiply(factorial(total.subtract(k_1)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(solution(java.math.BigInteger.valueOf(25))));
            System.out.println(_p(solution(java.math.BigInteger.valueOf(23))));
            System.out.println(_p(solution(java.math.BigInteger.valueOf(20))));
            System.out.println(_p(solution(java.math.BigInteger.valueOf(15))));
            System.out.println(_p(solution(java.math.BigInteger.valueOf(1))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
