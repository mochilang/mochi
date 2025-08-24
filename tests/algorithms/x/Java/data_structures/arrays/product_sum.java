public class Main {
    static Object[] example;

    static java.math.BigInteger product_sum(Object[] arr, java.math.BigInteger depth) {
        java.math.BigInteger total = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            Object el_1 = arr[(int)(((java.math.BigInteger)(i_1)).longValue())];
            if (((Boolean)(_exists(el_1)))) {
                total = new java.math.BigInteger(String.valueOf(total.add(product_sum(((Object[])(_toObjectArray(el_1))), new java.math.BigInteger(String.valueOf(depth.add(java.math.BigInteger.valueOf(1))))))));
            } else {
                total = new java.math.BigInteger(String.valueOf(total.add(new java.math.BigInteger(String.valueOf(((Number)(el_1)).intValue())))));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return total.multiply(depth);
    }

    static java.math.BigInteger product_sum_array(Object[] array) {
        java.math.BigInteger res = new java.math.BigInteger(String.valueOf(product_sum(((Object[])(array)), java.math.BigInteger.valueOf(1))));
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            example = new Object[]{5, 2, new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(7)).negate())), java.math.BigInteger.valueOf(1)}, 3, new Object[]{6, new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(13)).negate())), java.math.BigInteger.valueOf(8)}, 4}};
            System.out.println(product_sum_array(((Object[])(example))));
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

    static boolean _exists(Object v) {
        if (v == null) return false;
        if (v instanceof java.util.List<?>) return true;
        return v.getClass().isArray();
    }

    static Object[] _toObjectArray(Object v) {
        if (v instanceof Object[]) return (Object[]) v;
        if (v instanceof int[]) return java.util.Arrays.stream((int[]) v).boxed().toArray();
        if (v instanceof double[]) return java.util.Arrays.stream((double[]) v).boxed().toArray();
        if (v instanceof long[]) return java.util.Arrays.stream((long[]) v).boxed().toArray();
        if (v instanceof boolean[]) { boolean[] a = (boolean[]) v; Object[] out = new Object[a.length]; for (int i = 0; i < a.length; i++) out[i] = a[i]; return out; }
        return (Object[]) v;
    }
}
