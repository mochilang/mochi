public class Main {
    static double[][] tableau = new double[0][];
    static double[][] finalTab = new double[0][];
    static java.util.Map<String,Double> res;

    static double[][] pivot(double[][] t, long row, long col) {
        double[] pivotRow = ((double[])(new double[]{}));
        double pivotVal_1 = (double)(t[(int)((long)(row))][(int)((long)(col))]);
        for (int j = 0; j < t[(int)((long)(row))].length; j++) {
            pivotRow = ((double[])(appendDouble(pivotRow, (double)((double)(t[(int)((long)(row))][(int)((long)(j))]) / (double)(pivotVal_1)))));
        }
t[(int)((long)(row))] = ((double[])(pivotRow));
        for (int i = 0; i < t.length; i++) {
            if ((long)(i) != (long)(row)) {
                double factor_1 = (double)(t[(int)((long)(i))][(int)((long)(col))]);
                double[] newRow_1 = ((double[])(new double[]{}));
                for (int j = 0; j < t[(int)((long)(i))].length; j++) {
                    double value_1 = (double)((double)(t[(int)((long)(i))][(int)((long)(j))]) - (double)((double)(factor_1) * (double)(pivotRow[(int)((long)(j))])));
                    newRow_1 = ((double[])(appendDouble(newRow_1, (double)(value_1))));
                }
t[(int)((long)(i))] = ((double[])(newRow_1));
            }
        }
        return t;
    }

    static long[] findPivot(double[][] t) {
        long col = 0L;
        double minVal_1 = (double)(0.0);
        for (int j = 0; j < (long)(t[(int)(0L)].length) - 1L; j++) {
            double v_1 = (double)(t[(int)(0L)][(int)((long)(j))]);
            if ((double)(v_1) < (double)(minVal_1)) {
                minVal_1 = (double)(v_1);
                col = (long)(j);
            }
        }
        if ((double)(minVal_1) >= (double)(0.0)) {
            return new long[]{-1, -1};
        }
        long row_1 = (long)(-1);
        double minRatio_1 = (double)(0.0);
        boolean first_1 = true;
        for (int i = 1; i < t.length; i++) {
            double coeff_1 = (double)(t[(int)((long)(i))][(int)((long)(col))]);
            if ((double)(coeff_1) > (double)(0.0)) {
                double rhs_1 = (double)(t[(int)((long)(i))][(int)((long)((long)(t[(int)((long)(i))].length) - 1L))]);
                double ratio_1 = (double)((double)(rhs_1) / (double)(coeff_1));
                if (first_1 || (double)(ratio_1) < (double)(minRatio_1)) {
                    minRatio_1 = (double)(ratio_1);
                    row_1 = (long)(i);
                    first_1 = false;
                }
            }
        }
        return new long[]{row_1, col};
    }

    static java.util.Map<String,Double> interpret(double[][] t, long nVars) {
        long lastCol = (long)((long)(t[(int)(0L)].length) - 1L);
        double p_1 = (double)(t[(int)(0L)][(int)((long)(lastCol))]);
        if ((double)(p_1) < (double)(0.0)) {
            p_1 = (double)(-p_1);
        }
        java.util.Map<String,Double> result_1 = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
result_1.put("P", (double)(p_1));
        for (int i = 0; i < nVars; i++) {
            long nzRow_1 = (long)(-1);
            long nzCount_1 = 0L;
            for (int r = 0; r < t.length; r++) {
                double val_1 = (double)(t[(int)((long)(r))][(int)((long)(i))]);
                if ((double)(val_1) != (double)(0.0)) {
                    nzCount_1 = (long)((long)(nzCount_1) + 1L);
                    nzRow_1 = (long)(r);
                }
            }
            if ((long)(nzCount_1) == 1L && (double)(t[(int)((long)(nzRow_1))][(int)((long)(i))]) == (double)(1.0)) {
result_1.put("x" + _p((long)(i) + 1L), (double)(t[(int)((long)(nzRow_1))][(int)((long)(lastCol))]));
            }
        }
        return result_1;
    }

    static double[][] simplex(double[][] tab) {
        double[][] t = ((double[][])(tab));
        while (true) {
            long[] p_3 = ((long[])(findPivot(((double[][])(t)))));
            long row_3 = (long)(p_3[(int)(0L)]);
            long col_2 = (long)(p_3[(int)(1L)]);
            if ((long)(row_3) < 0L) {
                break;
            }
            t = ((double[][])(pivot(((double[][])(t)), (long)(row_3), (long)(col_2))));
        }
        return t;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            tableau = ((double[][])(new double[][]{new double[]{-1.0, -1.0, 0.0, 0.0, 0.0}, new double[]{1.0, 3.0, 1.0, 0.0, 4.0}, new double[]{3.0, 1.0, 0.0, 1.0, 4.0}}));
            finalTab = ((double[][])(simplex(((double[][])(tableau)))));
            res = interpret(((double[][])(finalTab)), 2L);
            System.out.println("P: " + _p(((double)(res).getOrDefault("P", 0.0))));
            for (int i = 0; i < 2; i++) {
                String key = "x" + _p((long)(i) + 1L);
                if (res.containsKey(key)) {
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
