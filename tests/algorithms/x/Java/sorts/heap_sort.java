public class Main {
    static java.math.BigInteger[] data = new java.math.BigInteger[0];
    static java.math.BigInteger[] result = new java.math.BigInteger[0];

    static void heapify(java.math.BigInteger[] arr, java.math.BigInteger index, java.math.BigInteger heap_size) {
        java.math.BigInteger largest = index;
        java.math.BigInteger left_index_1 = java.math.BigInteger.valueOf(2).multiply(index).add(java.math.BigInteger.valueOf(1));
        java.math.BigInteger right_index_1 = java.math.BigInteger.valueOf(2).multiply(index).add(java.math.BigInteger.valueOf(2));
        if (left_index_1.compareTo(heap_size) < 0 && arr[(int)(((java.math.BigInteger)(left_index_1)).longValue())].compareTo(arr[(int)(((java.math.BigInteger)(largest)).longValue())]) > 0) {
            largest = left_index_1;
        }
        if (right_index_1.compareTo(heap_size) < 0 && arr[(int)(((java.math.BigInteger)(right_index_1)).longValue())].compareTo(arr[(int)(((java.math.BigInteger)(largest)).longValue())]) > 0) {
            largest = right_index_1;
        }
        if (largest.compareTo(index) != 0) {
            java.math.BigInteger temp_1 = arr[(int)(((java.math.BigInteger)(largest)).longValue())];
arr[(int)(((java.math.BigInteger)(largest)).longValue())] = arr[(int)(((java.math.BigInteger)(index)).longValue())];
arr[(int)(((java.math.BigInteger)(index)).longValue())] = temp_1;
            heapify(((java.math.BigInteger[])(arr)), largest, heap_size);
        }
    }

    static java.math.BigInteger[] heap_sort(java.math.BigInteger[] arr) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(arr.length));
        java.math.BigInteger i_1 = n.divide(java.math.BigInteger.valueOf(2)).subtract(java.math.BigInteger.valueOf(1));
        while (i_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            heapify(((java.math.BigInteger[])(arr)), i_1, n);
            i_1 = i_1.subtract(java.math.BigInteger.valueOf(1));
        }
        i_1 = n.subtract(java.math.BigInteger.valueOf(1));
        while (i_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger temp_3 = arr[(int)(0L)];
arr[(int)(0L)] = arr[(int)(((java.math.BigInteger)(i_1)).longValue())];
arr[(int)(((java.math.BigInteger)(i_1)).longValue())] = temp_3;
            heapify(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(0), i_1);
            i_1 = i_1.subtract(java.math.BigInteger.valueOf(1));
        }
        return arr;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            data = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(28), java.math.BigInteger.valueOf(123), (java.math.BigInteger.valueOf(5)).negate(), java.math.BigInteger.valueOf(8), (java.math.BigInteger.valueOf(30)).negate(), (java.math.BigInteger.valueOf(200)).negate(), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(4)}));
            result = ((java.math.BigInteger[])(heap_sort(((java.math.BigInteger[])(data)))));
            System.out.println(java.util.Arrays.toString(result));
            if (!(_p(result).equals(_p(new java.math.BigInteger[]{(java.math.BigInteger.valueOf(200)).negate(), (java.math.BigInteger.valueOf(30)).negate(), (java.math.BigInteger.valueOf(5)).negate(), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(28), java.math.BigInteger.valueOf(123)})))) {
                throw new RuntimeException(String.valueOf("Assertion error"));
            }
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
