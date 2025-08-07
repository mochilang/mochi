public class Main {

    static void search_in_sorted_matrix(double[][] mat, int m, int n, double key) {
        int i = m - 1;
        int j = 0;
        while (i >= 0 && j < n) {
            if (key == mat[i][j]) {
                System.out.println("Key " + _p(key) + " found at row- " + _p(i + 1) + " column- " + _p(j + 1));
                return;
            }
            if (key < mat[i][j]) {
                i = i - 1;
            } else {
                j = j + 1;
            }
        }
        System.out.println("Key " + _p(key) + " not found");
    }

    static void main() {
        double[][] mat = ((double[][])(new double[][]{new double[]{2.0, 5.0, 7.0}, new double[]{4.0, 8.0, 13.0}, new double[]{9.0, 11.0, 15.0}, new double[]{12.0, 17.0, 20.0}}));
        search_in_sorted_matrix(((double[][])(mat)), mat.length, mat[0].length, 5.0);
        search_in_sorted_matrix(((double[][])(mat)), mat.length, mat[0].length, 21.0);
        double[][] mat2 = ((double[][])(new double[][]{new double[]{2.1, 5.0, 7.0}, new double[]{4.0, 8.0, 13.0}, new double[]{9.0, 11.0, 15.0}, new double[]{12.0, 17.0, 20.0}}));
        search_in_sorted_matrix(((double[][])(mat2)), mat2.length, mat2[0].length, 2.1);
        search_in_sorted_matrix(((double[][])(mat2)), mat2.length, mat2[0].length, 2.2);
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
