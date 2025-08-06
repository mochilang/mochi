public class Main {
    static double[][] tableau = new double[0][];
    static double[][] finalTab = new double[0][];
    static java.util.Map<String,Double> res;

    static double[][] pivot(double[][] t, int row, int col) {
        double[] pivotRow = ((double[])(new double[]{}));
        double pivotVal = t[row][col];
        for (int j = 0; j < t[row].length; j++) {
            pivotRow = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(pivotRow), java.util.stream.DoubleStream.of(t[row][j] / pivotVal)).toArray()));
        }
t[row] = ((double[])(pivotRow));
        for (int i = 0; i < t.length; i++) {
            if (i != row) {
                double factor = t[i][col];
                double[] newRow = ((double[])(new double[]{}));
                for (int j = 0; j < t[i].length; j++) {
                    double value = t[i][j] - factor * pivotRow[j];
                    newRow = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(newRow), java.util.stream.DoubleStream.of(value)).toArray()));
                }
t[i] = ((double[])(newRow));
            }
        }
        return t;
    }

    static int[] findPivot(double[][] t) {
        int col = 0;
        double minVal = 0.0;
        for (int j = 0; j < t[0].length - 1; j++) {
            double v = t[0][j];
            if (v < minVal) {
                minVal = v;
                col = j;
            }
        }
        if (minVal >= 0.0) {
            return new int[]{-1, -1};
        }
        int row = -1;
        double minRatio = 0.0;
        boolean first = true;
        for (int i = 1; i < t.length; i++) {
            double coeff = t[i][col];
            if (coeff > 0.0) {
                double rhs = t[i][t[i].length - 1];
                double ratio = rhs / coeff;
                if (first || ratio < minRatio) {
                    minRatio = ratio;
                    row = i;
                    first = false;
                }
            }
        }
        return new int[]{row, col};
    }

    static java.util.Map<String,Double> interpret(double[][] t, int nVars) {
        int lastCol = t[0].length - 1;
        double p = t[0][lastCol];
        if (p < 0.0) {
            p = -p;
        }
        java.util.Map<String,Double> result = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
result.put("P", p);
        for (int i = 0; i < nVars; i++) {
            int nzRow = -1;
            int nzCount = 0;
            for (int r = 0; r < t.length; r++) {
                double val = t[r][i];
                if (val != 0.0) {
                    nzCount = nzCount + 1;
                    nzRow = r;
                }
            }
            if (nzCount == 1 && t[nzRow][i] == 1.0) {
result.put("x" + _p(i + 1), t[nzRow][lastCol]);
            }
        }
        return result;
    }

    static double[][] simplex(double[][] tab) {
        double[][] t = ((double[][])(tab));
        while (true) {
            int[] p_1 = ((int[])(findPivot(((double[][])(t)))));
            int row_1 = p_1[0];
            int col_1 = p_1[1];
            if (row_1 < 0) {
                break;
            }
            t = ((double[][])(pivot(((double[][])(t)), row_1, col_1)));
        }
        return t;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            tableau = ((double[][])(new double[][]{new double[]{-1.0, -1.0, 0.0, 0.0, 0.0}, new double[]{1.0, 3.0, 1.0, 0.0, 4.0}, new double[]{3.0, 1.0, 0.0, 1.0, 4.0}}));
            finalTab = ((double[][])(simplex(((double[][])(tableau)))));
            res = interpret(((double[][])(finalTab)), 2);
            System.out.println("P: " + _p(((double)(res).getOrDefault("P", 0.0))));
            for (int i = 0; i < 2; i++) {
                String key = "x" + _p(i + 1);
                if (((Boolean)(res.containsKey(key)))) {
                    System.out.println(key + ": " + _p(((double)(res).getOrDefault(key, 0.0))));
                }
            }
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
