public class Main {
    static class MatrixChainResult {
        int[][] matrix;
        int[][] solution;
        MatrixChainResult(int[][] matrix, int[][] solution) {
            this.matrix = matrix;
            this.solution = solution;
        }
        MatrixChainResult() {}
        @Override public String toString() {
            return String.format("{'matrix': %s, 'solution': %s}", String.valueOf(matrix), String.valueOf(solution));
        }
    }


    static int[][] make_2d(int n) {
        int[][] res = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < n) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j = j + 1;
            }
            res = ((int[][])(appendObj(res, row)));
            i = i + 1;
        }
        return res;
    }

    static MatrixChainResult matrix_chain_order(int[] arr) {
        int n = arr.length;
        int[][] m = ((int[][])(make_2d(n)));
        int[][] s = ((int[][])(make_2d(n)));
        int chain_length = 2;
        while (chain_length < n) {
            int a = 1;
            while (a < n - chain_length + 1) {
                int b = a + chain_length - 1;
m[a][b] = 1000000000;
                int c = a;
                while (c < b) {
                    int cost = m[a][c] + m[c + 1][b] + arr[a - 1] * arr[c] * arr[b];
                    if (cost < m[a][b]) {
m[a][b] = cost;
s[a][b] = c;
                    }
                    c = c + 1;
                }
                a = a + 1;
            }
            chain_length = chain_length + 1;
        }
        return new MatrixChainResult(m, s);
    }

    static String optimal_parenthesization(int[][] s, int i, int j) {
        if (i == j) {
            return "A" + _p(i);
        } else {
            String left = String.valueOf(optimal_parenthesization(((int[][])(s)), i, s[i][j]));
            String right = String.valueOf(optimal_parenthesization(((int[][])(s)), s[i][j] + 1, j));
            return "( " + left + " " + right + " )";
        }
    }

    static void main() {
        int[] arr = ((int[])(new int[]{30, 35, 15, 5, 10, 20, 25}));
        int n_1 = arr.length;
        MatrixChainResult res_1 = matrix_chain_order(((int[])(arr)));
        int[][] m_1 = ((int[][])(res_1.matrix));
        int[][] s_1 = ((int[][])(res_1.solution));
        System.out.println("No. of Operation required: " + _p(_geti(m_1[1], n_1 - 1)));
        String seq = String.valueOf(optimal_parenthesization(((int[][])(s_1)), 1, n_1 - 1));
        System.out.println(seq);
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
