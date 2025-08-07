public class Main {

    static int[][] multiply(int[][] matrix_a, int[][] matrix_b) {
        int n = matrix_a.length;
        int[][] matrix_c = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < n) {
                int val = 0;
                int k = 0;
                while (k < n) {
                    val = val + matrix_a[i][k] * matrix_b[k][j];
                    k = k + 1;
                }
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(val)).toArray()));
                j = j + 1;
            }
            matrix_c = ((int[][])(appendObj(matrix_c, row)));
            i = i + 1;
        }
        return matrix_c;
    }

    static int[][] identity(int n) {
        int[][] res = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < n) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < n) {
                if (i_1 == j_1) {
                    row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(1)).toArray()));
                } else {
                    row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(0)).toArray()));
                }
                j_1 = j_1 + 1;
            }
            res = ((int[][])(appendObj(res, row_1)));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int nth_fibonacci_matrix(int n) {
        if (n <= 1) {
            return n;
        }
        int[][] res_matrix = ((int[][])(identity(2)));
        int[][] fib_matrix = ((int[][])(new int[][]{new int[]{1, 1}, new int[]{1, 0}}));
        int m = n - 1;
        while (m > 0) {
            if (Math.floorMod(m, 2) == 1) {
                res_matrix = ((int[][])(multiply(((int[][])(res_matrix)), ((int[][])(fib_matrix)))));
            }
            fib_matrix = ((int[][])(multiply(((int[][])(fib_matrix)), ((int[][])(fib_matrix)))));
            m = Math.floorDiv(m, 2);
        }
        return res_matrix[0][0];
    }

    static int nth_fibonacci_bruteforce(int n) {
        if (n <= 1) {
            return n;
        }
        int fib0 = 0;
        int fib1 = 1;
        int i_2 = 2;
        while (i_2 <= n) {
            int next = fib0 + fib1;
            fib0 = fib1;
            fib1 = next;
            i_2 = i_2 + 1;
        }
        return fib1;
    }

    static int parse_number(String s) {
        int result = 0;
        int i_3 = 0;
        while (i_3 < _runeLen(s)) {
            String ch = _substr(s, i_3, i_3 + 1);
            if ((ch.compareTo("0") >= 0) && (ch.compareTo("9") <= 0)) {
                result = result * 10 + (Integer.parseInt(ch));
            }
            i_3 = i_3 + 1;
        }
        return result;
    }

    static void main() {
        String[] ordinals = ((String[])(new String[]{"0th", "1st", "2nd", "3rd", "10th", "100th", "1000th"}));
        int i_4 = 0;
        while (i_4 < ordinals.length) {
            String ordinal = ordinals[i_4];
            int n_1 = parse_number(ordinal);
            String msg = ordinal + " fibonacci number using matrix exponentiation is " + _p(nth_fibonacci_matrix(n_1)) + " and using bruteforce is " + _p(nth_fibonacci_bruteforce(n_1));
            System.out.println(msg);
            i_4 = i_4 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
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
        return String.valueOf(v);
    }
}
