public class Main {

    static java.math.BigInteger[] merge(java.math.BigInteger[] a, java.math.BigInteger low, java.math.BigInteger mid, java.math.BigInteger high) {
        java.math.BigInteger[] left = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(a, (int)(((java.math.BigInteger)(low)).longValue()), (int)(((java.math.BigInteger)(mid)).longValue()))));
        java.math.BigInteger[] right_1 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(a, (int)(((java.math.BigInteger)(mid)).longValue()), (int)(((java.math.BigInteger)(high.add(java.math.BigInteger.valueOf(1)))).longValue()))));
        java.math.BigInteger[] result_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        while (new java.math.BigInteger(String.valueOf(left.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0 && new java.math.BigInteger(String.valueOf(right_1.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            if (left[(int)(0L)].compareTo(right_1[(int)(0L)]) <= 0) {
                result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(left[(int)(0L)])).toArray(java.math.BigInteger[]::new)));
                left = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(left, (int)(1L), (int)((long)(left.length)))));
            } else {
                result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(right_1[(int)(0L)])).toArray(java.math.BigInteger[]::new)));
                right_1 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(right_1, (int)(1L), (int)((long)(right_1.length)))));
            }
        }
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(left.length))) < 0) {
            result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(left[(int)(((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(right_1.length))) < 0) {
            result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(right_1[(int)(((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(result_1.length))) < 0) {
a[(int)(((java.math.BigInteger)(low.add(i_1))).longValue())] = result_1[(int)(((java.math.BigInteger)(i_1)).longValue())];
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return a;
    }

    static java.math.BigInteger[] iter_merge_sort(java.math.BigInteger[] items) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(items.length));
        if (n.compareTo(java.math.BigInteger.valueOf(1)) <= 0) {
            return items;
        }
        java.math.BigInteger[] arr_1 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(items, (int)(0L), (int)((long)(items.length)))));
        java.math.BigInteger p_1 = java.math.BigInteger.valueOf(2);
        while (p_1.compareTo(n) <= 0) {
            java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
            while (i_3.compareTo(n) < 0) {
                java.math.BigInteger high_1 = i_3.add(p_1).subtract(java.math.BigInteger.valueOf(1));
                if (high_1.compareTo(n) >= 0) {
                    high_1 = n.subtract(java.math.BigInteger.valueOf(1));
                }
                java.math.BigInteger low_1 = i_3;
                java.math.BigInteger mid_1 = (low_1.add(high_1).add(java.math.BigInteger.valueOf(1))).divide(java.math.BigInteger.valueOf(2));
                arr_1 = ((java.math.BigInteger[])(merge(((java.math.BigInteger[])(arr_1)), low_1, mid_1, high_1)));
                i_3 = i_3.add(p_1);
            }
            if (p_1.multiply(java.math.BigInteger.valueOf(2)).compareTo(n) >= 0) {
                java.math.BigInteger mid2_1 = i_3.subtract(p_1);
                arr_1 = ((java.math.BigInteger[])(merge(((java.math.BigInteger[])(arr_1)), java.math.BigInteger.valueOf(0), mid2_1, n.subtract(java.math.BigInteger.valueOf(1)))));
                break;
            }
            p_1 = p_1.multiply(java.math.BigInteger.valueOf(2));
        }
        return arr_1;
    }

    static String list_to_string(java.math.BigInteger[] arr) {
        String s = "[";
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            s = s + _p(_geto(arr, ((Number)(i_5)).intValue()));
            if (i_5.compareTo(new java.math.BigInteger(String.valueOf(arr.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                s = s + ", ";
            }
            i_5 = i_5.add(java.math.BigInteger.valueOf(1));
        }
        return s + "]";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(list_to_string(((java.math.BigInteger[])(iter_merge_sort(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(7)})))))));
            System.out.println(list_to_string(((java.math.BigInteger[])(iter_merge_sort(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1)})))))));
            System.out.println(list_to_string(((java.math.BigInteger[])(iter_merge_sort(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1)})))))));
            System.out.println(list_to_string(((java.math.BigInteger[])(iter_merge_sort(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1)})))))));
            System.out.println(list_to_string(((java.math.BigInteger[])(iter_merge_sort(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1)})))))));
            System.out.println(list_to_string(((java.math.BigInteger[])(iter_merge_sort(((java.math.BigInteger[])(new java.math.BigInteger[]{(java.math.BigInteger.valueOf(2)).negate(), (java.math.BigInteger.valueOf(9)).negate(), (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(4)).negate()})))))));
            System.out.println(list_to_string(((java.math.BigInteger[])(iter_merge_sort(((java.math.BigInteger[])(new java.math.BigInteger[]{})))))));
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
