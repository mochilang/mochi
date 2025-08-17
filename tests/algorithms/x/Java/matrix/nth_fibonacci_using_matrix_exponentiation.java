public class Main {

    static long[][] multiply(long[][] matrix_a, long[][] matrix_b) {
        long n = (long)(matrix_a.length);
        long[][] matrix_c_1 = ((long[][])(new long[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(n)) {
                long val_1 = 0L;
                long k_1 = 0L;
                while ((long)(k_1) < (long)(n)) {
                    val_1 = (long)((long)(val_1) + (long)((long)(matrix_a[(int)((long)(i_1))][(int)((long)(k_1))]) * (long)(matrix_b[(int)((long)(k_1))][(int)((long)(j_1))])));
                    k_1 = (long)((long)(k_1) + 1L);
                }
                row_1 = ((long[])(appendLong(row_1, (long)(val_1))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            matrix_c_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix_c_1), java.util.stream.Stream.of(new long[][]{row_1})).toArray(long[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return matrix_c_1;
    }

    static long[][] identity(long n) {
        long[][] res = ((long[][])(new long[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n)) {
            long[] row_3 = ((long[])(new long[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(n)) {
                if ((long)(i_3) == (long)(j_3)) {
                    row_3 = ((long[])(appendLong(row_3, 1L)));
                } else {
                    row_3 = ((long[])(appendLong(row_3, 0L)));
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            res = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(new long[][]{row_3})).toArray(long[][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res;
    }

    static long nth_fibonacci_matrix(long n) {
        if ((long)(n) <= 1L) {
            return n;
        }
        long[][] res_matrix_1 = ((long[][])(identity(2L)));
        long[][] fib_matrix_1 = ((long[][])(new long[][]{new long[]{1, 1}, new long[]{1, 0}}));
        long m_1 = (long)((long)(n) - 1L);
        while ((long)(m_1) > 0L) {
            if (Math.floorMod(m_1, 2) == 1L) {
                res_matrix_1 = ((long[][])(multiply(((long[][])(res_matrix_1)), ((long[][])(fib_matrix_1)))));
            }
            fib_matrix_1 = ((long[][])(multiply(((long[][])(fib_matrix_1)), ((long[][])(fib_matrix_1)))));
            m_1 = Math.floorDiv(m_1, 2);
        }
        return res_matrix_1[(int)(0L)][(int)(0L)];
    }

    static long nth_fibonacci_bruteforce(long n) {
        if ((long)(n) <= 1L) {
            return n;
        }
        long fib0_1 = 0L;
        long fib1_1 = 1L;
        long i_5 = 2L;
        while ((long)(i_5) <= (long)(n)) {
            long next_1 = (long)((long)(fib0_1) + (long)(fib1_1));
            fib0_1 = (long)(fib1_1);
            fib1_1 = (long)(next_1);
            i_5 = (long)((long)(i_5) + 1L);
        }
        return fib1_1;
    }

    static long parse_number(String s) {
        long result = 0L;
        long i_7 = 0L;
        while ((long)(i_7) < (long)(_runeLen(s))) {
            String ch_1 = _substr(s, (int)((long)(i_7)), (int)((long)((long)(i_7) + 1L)));
            if ((ch_1.compareTo("0") >= 0) && (ch_1.compareTo("9") <= 0)) {
                result = (long)((long)((long)(result) * 10L) + (long)((Integer.parseInt(ch_1))));
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return result;
    }

    static void main() {
        String[] ordinals = ((String[])(new String[]{"0th", "1st", "2nd", "3rd", "10th", "100th", "1000th"}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(ordinals.length)) {
            String ordinal_1 = ordinals[(int)((long)(i_9))];
            long n_2 = (long)(parse_number(ordinal_1));
            String msg_1 = ordinal_1 + " fibonacci number using matrix exponentiation is " + _p(nth_fibonacci_matrix((long)(n_2))) + " and using bruteforce is " + _p(nth_fibonacci_bruteforce((long)(n_2)));
            System.out.println(msg_1);
            i_9 = (long)((long)(i_9) + 1L);
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
