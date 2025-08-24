public class Main {

    static java.math.BigInteger[] sort_triplet(java.math.BigInteger a, java.math.BigInteger b, java.math.BigInteger c) {
        java.math.BigInteger x = new java.math.BigInteger(String.valueOf(a));
        java.math.BigInteger y_1 = new java.math.BigInteger(String.valueOf(b));
        java.math.BigInteger z_1 = new java.math.BigInteger(String.valueOf(c));
        if (x.compareTo(y_1) > 0) {
            java.math.BigInteger t_1 = new java.math.BigInteger(String.valueOf(x));
            x = new java.math.BigInteger(String.valueOf(y_1));
            y_1 = new java.math.BigInteger(String.valueOf(t_1));
        }
        if (y_1.compareTo(z_1) > 0) {
            java.math.BigInteger t_3 = new java.math.BigInteger(String.valueOf(y_1));
            y_1 = new java.math.BigInteger(String.valueOf(z_1));
            z_1 = new java.math.BigInteger(String.valueOf(t_3));
        }
        if (x.compareTo(y_1) > 0) {
            java.math.BigInteger t_5 = new java.math.BigInteger(String.valueOf(x));
            x = new java.math.BigInteger(String.valueOf(y_1));
            y_1 = new java.math.BigInteger(String.valueOf(t_5));
        }
        return new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(x)), new java.math.BigInteger(String.valueOf(y_1)), new java.math.BigInteger(String.valueOf(z_1))};
    }

    static boolean contains_triplet(java.math.BigInteger[][] arr, java.math.BigInteger[] target) {
        for (int i = 0; i < arr.length; i++) {
            java.math.BigInteger[] item = ((java.math.BigInteger[])(arr[(int)((long)(i))]));
            boolean same = true;
            for (int j = 0; j < target.length; j++) {
                if (item[(int)((long)(j))].compareTo(target[(int)((long)(j))]) != 0) {
                    same = false;
                    break;
                }
            }
            if (same) {
                return true;
            }
        }
        return false;
    }

    static boolean contains_int(java.math.BigInteger[] arr, java.math.BigInteger value) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[(int)((long)(i))].compareTo(value) == 0) {
                return true;
            }
        }
        return false;
    }

    static java.math.BigInteger[][] find_triplets_with_0_sum(java.math.BigInteger[] nums) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(nums.length));
        java.math.BigInteger[][] result_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        for (int i = 0; i < n; i++) {
            for (int j = (new java.math.BigInteger(String.valueOf(i)).add(java.math.BigInteger.valueOf(1))); j < n; j++) {
                for (int k = (new java.math.BigInteger(String.valueOf(j)).add(java.math.BigInteger.valueOf(1))); k < n; k++) {
                    java.math.BigInteger a_1 = new java.math.BigInteger(String.valueOf(nums[(int)((long)(i))]));
                    java.math.BigInteger b_1 = new java.math.BigInteger(String.valueOf(nums[(int)((long)(j))]));
                    java.math.BigInteger c_1 = new java.math.BigInteger(String.valueOf(nums[(int)((long)(k))]));
                    if (a_1.add(b_1).add(c_1).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                        java.math.BigInteger[] trip_1 = ((java.math.BigInteger[])(sort_triplet(new java.math.BigInteger(String.valueOf(a_1)), new java.math.BigInteger(String.valueOf(b_1)), new java.math.BigInteger(String.valueOf(c_1)))));
                        if (!(Boolean)contains_triplet(((java.math.BigInteger[][])(result_1)), ((java.math.BigInteger[])(trip_1)))) {
                            result_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{trip_1})).toArray(java.math.BigInteger[][]::new)));
                        }
                    }
                }
            }
        }
        return result_1;
    }

    static java.math.BigInteger[][] find_triplets_with_0_sum_hashing(java.math.BigInteger[] arr) {
        java.math.BigInteger target_sum = java.math.BigInteger.valueOf(0);
        java.math.BigInteger[][] output_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        for (int i = 0; i < arr.length; i++) {
            java.math.BigInteger[] seen_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger current_sum_1 = new java.math.BigInteger(String.valueOf(target_sum.subtract(arr[(int)((long)(i))])));
            for (int j = (new java.math.BigInteger(String.valueOf(i)).add(java.math.BigInteger.valueOf(1))); j < arr.length; j++) {
                java.math.BigInteger other_1 = new java.math.BigInteger(String.valueOf(arr[(int)((long)(j))]));
                java.math.BigInteger required_1 = new java.math.BigInteger(String.valueOf(current_sum_1.subtract(other_1)));
                if (contains_int(((java.math.BigInteger[])(seen_1)), new java.math.BigInteger(String.valueOf(required_1)))) {
                    java.math.BigInteger[] trip_3 = ((java.math.BigInteger[])(sort_triplet(new java.math.BigInteger(String.valueOf(arr[(int)((long)(i))])), new java.math.BigInteger(String.valueOf(other_1)), new java.math.BigInteger(String.valueOf(required_1)))));
                    if (!(Boolean)contains_triplet(((java.math.BigInteger[][])(output_1)), ((java.math.BigInteger[])(trip_3)))) {
                        output_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(output_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{trip_3})).toArray(java.math.BigInteger[][]::new)));
                    }
                }
                seen_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(seen_1), java.util.stream.Stream.of(other_1)).toArray(java.math.BigInteger[]::new)));
            }
        }
        return output_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(find_triplets_with_0_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(4)).negate()))})))));
            System.out.println(_p(find_triplets_with_0_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
            System.out.println(_p(find_triplets_with_0_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)})))));
            System.out.println(_p(find_triplets_with_0_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(3)).negate()))})))));
            System.out.println(_p(find_triplets_with_0_sum_hashing(((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(4)).negate()))})))));
            System.out.println(_p(find_triplets_with_0_sum_hashing(((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
            System.out.println(_p(find_triplets_with_0_sum_hashing(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)})))));
            System.out.println(_p(find_triplets_with_0_sum_hashing(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(3)).negate()))})))));
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
