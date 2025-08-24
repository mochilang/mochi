public class Main {

    static java.math.BigInteger[] tail(java.math.BigInteger[] xs) {
        java.math.BigInteger[] res = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            res = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(xs[(int)(((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return res;
    }

    static java.math.BigInteger[] rotate_left(java.math.BigInteger[] xs) {
        if (new java.math.BigInteger(String.valueOf(xs.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return xs;
        }
        java.math.BigInteger[] res_2 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            res_2 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(xs[(int)(((java.math.BigInteger)(i_3)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        res_2 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(xs[(int)(0L)])).toArray(java.math.BigInteger[]::new)));
        return res_2;
    }

    static java.math.BigInteger[][] permute_recursive(java.math.BigInteger[] nums) {
        if (new java.math.BigInteger(String.valueOf(nums.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            java.math.BigInteger[][] base = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
            return java.util.stream.Stream.concat(java.util.Arrays.stream(base), java.util.stream.Stream.of(new java.math.BigInteger[][]{new java.math.BigInteger[]{}})).toArray(java.math.BigInteger[][]::new);
        }
        java.math.BigInteger[][] result_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger[] current_1 = ((java.math.BigInteger[])(nums));
        java.math.BigInteger count_1 = java.math.BigInteger.valueOf(0);
        while (count_1.compareTo(new java.math.BigInteger(String.valueOf(nums.length))) < 0) {
            java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(current_1[(int)(0L)]));
            java.math.BigInteger[] rest_1 = ((java.math.BigInteger[])(tail(((java.math.BigInteger[])(current_1)))));
            java.math.BigInteger[][] perms_1 = ((java.math.BigInteger[][])(permute_recursive(((java.math.BigInteger[])(rest_1)))));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(perms_1.length))) < 0) {
                java.math.BigInteger[] perm_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(perms_1[(int)(((java.math.BigInteger)(j_1)).longValue())]), java.util.stream.Stream.of(n_1)).toArray(java.math.BigInteger[]::new)));
                result_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{perm_1})).toArray(java.math.BigInteger[][]::new)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            current_1 = ((java.math.BigInteger[])(rotate_left(((java.math.BigInteger[])(current_1)))));
            count_1 = new java.math.BigInteger(String.valueOf(count_1.add(java.math.BigInteger.valueOf(1))));
        }
        return result_1;
    }

    static java.math.BigInteger[] swap(java.math.BigInteger[] xs, java.math.BigInteger i, java.math.BigInteger j) {
        java.math.BigInteger[] res_3 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
        while (k_1.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            if (k_1.compareTo(i) == 0) {
                res_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(xs[(int)(((java.math.BigInteger)(j)).longValue())])).toArray(java.math.BigInteger[]::new)));
            } else             if (k_1.compareTo(j) == 0) {
                res_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(xs[(int)(((java.math.BigInteger)(i)).longValue())])).toArray(java.math.BigInteger[]::new)));
            } else {
                res_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(xs[(int)(((java.math.BigInteger)(k_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            }
            k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
        }
        return res_3;
    }

    static java.math.BigInteger[][] permute_backtrack_helper(java.math.BigInteger[] nums, java.math.BigInteger start, java.math.BigInteger[][] output) {
        if (start.compareTo(new java.math.BigInteger(String.valueOf(nums.length)).subtract(java.math.BigInteger.valueOf(1))) == 0) {
            return java.util.stream.Stream.concat(java.util.Arrays.stream(output), java.util.stream.Stream.of(new java.math.BigInteger[][]{nums})).toArray(java.math.BigInteger[][]::new);
        }
        java.math.BigInteger i_5 = new java.math.BigInteger(String.valueOf(start));
        java.math.BigInteger[][] res_5 = ((java.math.BigInteger[][])(output));
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(nums.length))) < 0) {
            java.math.BigInteger[] swapped_1 = ((java.math.BigInteger[])(swap(((java.math.BigInteger[])(nums)), new java.math.BigInteger(String.valueOf(start)), new java.math.BigInteger(String.valueOf(i_5)))));
            res_5 = ((java.math.BigInteger[][])(permute_backtrack_helper(((java.math.BigInteger[])(swapped_1)), new java.math.BigInteger(String.valueOf(start.add(java.math.BigInteger.valueOf(1)))), ((java.math.BigInteger[][])(res_5)))));
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return res_5;
    }

    static java.math.BigInteger[][] permute_backtrack(java.math.BigInteger[] nums) {
        java.math.BigInteger[][] output = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        return permute_backtrack_helper(((java.math.BigInteger[])(nums)), java.math.BigInteger.valueOf(0), ((java.math.BigInteger[][])(output)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(permute_recursive(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})))));
            System.out.println(_p(permute_backtrack(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})))));
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
