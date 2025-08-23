public class Main {

    static java.math.BigInteger power_of_two(java.math.BigInteger exp) {
        java.math.BigInteger result = java.math.BigInteger.valueOf(1);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(exp) < 0) {
            result = result.multiply(java.math.BigInteger.valueOf(2));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return result;
    }

    static java.math.BigInteger solution(java.math.BigInteger power) {
        java.math.BigInteger num = power_of_two(power);
        String string_num_1 = _p(num);
        java.math.BigInteger sum_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(string_num_1)))) < 0) {
            sum_1 = sum_1.add((new java.math.BigInteger(string_num_1.substring((int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3)).longValue())+1))));
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        return sum_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(solution(java.math.BigInteger.valueOf(1000))));
            System.out.println(_p(solution(java.math.BigInteger.valueOf(50))));
            System.out.println(_p(solution(java.math.BigInteger.valueOf(20))));
            System.out.println(_p(solution(java.math.BigInteger.valueOf(15))));
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
