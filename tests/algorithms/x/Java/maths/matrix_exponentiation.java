public class Main {

    static long[][] identity(long n) {
        long i = 0;
        long[][] mat_1 = ((long[][])(new long[][]{}));
        while (i < n) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_1 = 0;
            while (j_1 < n) {
                if (i == j_1) {
                    row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of(1)).toArray()));
                } else {
                    row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of(0)).toArray()));
                }
                j_1 = j_1 + 1;
            }
            mat_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat_1), java.util.stream.Stream.of(row_1)).toArray(long[][]::new)));
            i = i + 1;
        }
        return mat_1;
    }

    static long[][] matrix_mul(long[][] a, long[][] b) {
        long n = a.length;
        long[][] result_1 = ((long[][])(new long[][]{}));
        long i_2 = 0;
        while (i_2 < n) {
            long[] row_3 = ((long[])(new long[]{}));
            long j_3 = 0;
            while (j_3 < n) {
                long cell_1 = 0;
                long k_1 = 0;
                while (k_1 < n) {
                    cell_1 = cell_1 + a[(int)(i_2)][(int)(k_1)] * b[(int)(k_1)][(int)(j_3)];
                    k_1 = k_1 + 1;
                }
                row_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_3), java.util.stream.LongStream.of(cell_1)).toArray()));
                j_3 = j_3 + 1;
            }
            result_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(row_3)).toArray(long[][]::new)));
            i_2 = i_2 + 1;
        }
        return result_1;
    }

    static long[][] matrix_pow(long[][] base, long exp) {
        long[][] result_2 = ((long[][])(identity(base.length)));
        long[][] b_1 = ((long[][])(base));
        long e_1 = exp;
        while (e_1 > 0) {
            if (Math.floorMod(e_1, 2) == 1) {
                result_2 = ((long[][])(matrix_mul(((long[][])(result_2)), ((long[][])(b_1)))));
            }
            b_1 = ((long[][])(matrix_mul(((long[][])(b_1)), ((long[][])(b_1)))));
            e_1 = Math.floorDiv(e_1, 2);
        }
        return result_2;
    }

    static long fibonacci_with_matrix_exponentiation(long n, long f1, long f2) {
        if (n == 1) {
            return f1;
        }
        if (n == 2) {
            return f2;
        }
        long[][] base_1 = ((long[][])(new long[][]{new long[]{1, 1}, new long[]{1, 0}}));
        long[][] m_1 = ((long[][])(matrix_pow(((long[][])(base_1)), n - 2)));
        return f2 * m_1[(int)(0)][(int)(0)] + f1 * m_1[(int)(0)][(int)(1)];
    }

    static long simple_fibonacci(long n, long f1, long f2) {
        if (n == 1) {
            return f1;
        }
        if (n == 2) {
            return f2;
        }
        long a_1 = f1;
        long b_3 = f2;
        long count_1 = n - 2;
        while (count_1 > 0) {
            long tmp_1 = a_1 + b_3;
            a_1 = b_3;
            b_3 = tmp_1;
            count_1 = count_1 - 1;
        }
        return b_3;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(fibonacci_with_matrix_exponentiation(1, 5, 6)));
            System.out.println(_p(fibonacci_with_matrix_exponentiation(2, 10, 11)));
            System.out.println(_p(fibonacci_with_matrix_exponentiation(13, 0, 1)));
            System.out.println(_p(fibonacci_with_matrix_exponentiation(10, 5, 9)));
            System.out.println(_p(fibonacci_with_matrix_exponentiation(9, 2, 3)));
            System.out.println(_p(simple_fibonacci(1, 5, 6)));
            System.out.println(_p(simple_fibonacci(2, 10, 11)));
            System.out.println(_p(simple_fibonacci(13, 0, 1)));
            System.out.println(_p(simple_fibonacci(10, 5, 9)));
            System.out.println(_p(simple_fibonacci(9, 2, 3)));
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
