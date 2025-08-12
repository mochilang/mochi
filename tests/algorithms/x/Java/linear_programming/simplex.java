public class Main {
    static double[][] tableau = new double[0][];
    static double[][] finalTab = new double[0][];
    static java.util.Map<String,Double> res;

    static double[][] pivot(double[][] t, long row, long col) {
        double[] pivotRow = ((double[])(new double[]{}));
        double pivotVal_1 = t[(int)((long)(row))][(int)((long)(col))];
        for (int j = 0; j < t[(int)((long)(row))].length; j++) {
            pivotRow = ((double[])(appendDouble(pivotRow, t[(int)((long)(row))][(int)((long)(j))] / pivotVal_1)));
        }
t[(int)((long)(row))] = ((double[])(pivotRow));
        for (int i = 0; i < t.length; i++) {
            if (i != row) {
                double factor_1 = t[(int)((long)(i))][(int)((long)(col))];
                double[] newRow_1 = ((double[])(new double[]{}));
                for (int j = 0; j < t[(int)((long)(i))].length; j++) {
                    double value_1 = t[(int)((long)(i))][(int)((long)(j))] - factor_1 * pivotRow[(int)((long)(j))];
                    newRow_1 = ((double[])(appendDouble(newRow_1, value_1)));
                }
t[(int)((long)(i))] = ((double[])(newRow_1));
            }
        }
        return t;
    }

    static long[] findPivot(double[][] t) {
        long col = 0L;
        double minVal_1 = 0.0;
        for (int j = 0; j < t[(int)((long)(0))].length - 1; j++) {
            double v_1 = t[(int)((long)(0))][(int)((long)(j))];
            if (v_1 < minVal_1) {
                minVal_1 = v_1;
                col = j;
            }
        }
        if (minVal_1 >= 0.0) {
            return new long[]{-1, -1};
        }
        long row_1 = -1;
        double minRatio_1 = 0.0;
        boolean first_1 = true;
        for (int i = 1; i < t.length; i++) {
            double coeff_1 = t[(int)((long)(i))][(int)((long)(col))];
            if (coeff_1 > 0.0) {
                double rhs_1 = t[(int)((long)(i))][(int)((long)(t[(int)((long)(i))].length - 1))];
                double ratio_1 = rhs_1 / coeff_1;
                if (first_1 || ratio_1 < minRatio_1) {
                    minRatio_1 = ratio_1;
                    row_1 = i;
                    first_1 = false;
                }
            }
        }
        return new long[]{row_1, col};
    }

    static java.util.Map<String,Double> interpret(double[][] t, long nVars) {
        long lastCol = t[(int)((long)(0))].length - 1;
        double p_1 = t[(int)((long)(0))][(int)((long)(lastCol))];
        if (p_1 < 0.0) {
            p_1 = -p_1;
        }
        java.util.Map<String,Double> result_1 = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
result_1.put("P", p_1);
        for (int i = 0; i < nVars; i++) {
            long nzRow_1 = -1;
            long nzCount_1 = 0L;
            for (int r = 0; r < t.length; r++) {
                double val_1 = t[(int)((long)(r))][(int)((long)(i))];
                if (val_1 != 0.0) {
                    nzCount_1 = nzCount_1 + 1;
                    nzRow_1 = r;
                }
            }
            if (nzCount_1 == 1 && t[(int)((long)(nzRow_1))][(int)((long)(i))] == 1.0) {
result_1.put("x" + _p(i + 1), t[(int)((long)(nzRow_1))][(int)((long)(lastCol))]);
            }
        }
        return result_1;
    }

    static double[][] simplex(double[][] tab) {
        double[][] t = ((double[][])(tab));
        while (true) {
            long[] p_3 = ((long[])(findPivot(((double[][])(t)))));
            long row_3 = p_3[(int)((long)(0))];
            long col_2 = p_3[(int)((long)(1))];
            if (row_3 < 0) {
                break;
            }
            t = ((double[][])(pivot(((double[][])(t)), row_3, col_2)));
        }
        return t;
    }
    public static void main(String[] args) {
        tableau = ((double[][])(new double[][]{new double[]{-1.0, -1.0, 0.0, 0.0, 0.0}, new double[]{1.0, 3.0, 1.0, 0.0, 4.0}, new double[]{3.0, 1.0, 0.0, 1.0, 4.0}}));
        finalTab = ((double[][])(simplex(((double[][])(tableau)))));
        res = interpret(((double[][])(finalTab)), 2L);
        System.out.println("P: " + _p(((double)(res).getOrDefault("P", 0.0))));
        for (int i = 0; i < 2; i++) {
            String key = "x" + _p(i + 1);
            if (res.containsKey(key)) {
                System.out.println(key + ": " + _p(((double)(res).getOrDefault(key, 0.0))));
            }
        }
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
