public class Main {
    static double[][] vehicles = ((double[][])(new double[][]{}));
    static long[] weights = ((long[])(new long[]{0, 0, 1}));
    static double[][] result;

    static double[][] get_data(double[][] source_data) {
        double[][] data_lists = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(source_data.length)) {
            double[] row_1 = ((double[])(source_data[(int)((long)(i_1))]));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(row_1.length)) {
                if ((long)(data_lists.length) < (long)((long)(j_1) + 1L)) {
                    double[] empty_1 = ((double[])(new double[]{}));
                    data_lists = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(data_lists), java.util.stream.Stream.of(new double[][]{empty_1})).toArray(double[][]::new)));
                }
data_lists[(int)((long)(j_1))] = ((double[])(appendDouble(data_lists[(int)((long)(j_1))], (double)(row_1[(int)((long)(j_1))]))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return data_lists;
    }

    static double[][] calculate_each_score(double[][] data_lists, long[] weights) {
        double[][] score_lists = ((double[][])(new double[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(data_lists.length)) {
            double[] dlist_1 = ((double[])(data_lists[(int)((long)(i_3))]));
            long weight_1 = (long)(weights[(int)((long)(i_3))]);
            double mind_1 = (double)(dlist_1[(int)((long)(0))]);
            double maxd_1 = (double)(dlist_1[(int)((long)(0))]);
            long j_3 = 1L;
            while ((long)(j_3) < (long)(dlist_1.length)) {
                double val_1 = (double)(dlist_1[(int)((long)(j_3))]);
                if ((double)(val_1) < (double)(mind_1)) {
                    mind_1 = (double)(val_1);
                }
                if ((double)(val_1) > (double)(maxd_1)) {
                    maxd_1 = (double)(val_1);
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            double[] score_1 = ((double[])(new double[]{}));
            j_3 = 0L;
            if ((long)(weight_1) == 0L) {
                while ((long)(j_3) < (long)(dlist_1.length)) {
                    double item_2 = (double)(dlist_1[(int)((long)(j_3))]);
                    if ((double)((double)(maxd_1) - (double)(mind_1)) == (double)(0.0)) {
                        score_1 = ((double[])(appendDouble(score_1, (double)(1.0))));
                    } else {
                        score_1 = ((double[])(appendDouble(score_1, (double)((double)(1.0) - (double)(((double)(((double)(item_2) - (double)(mind_1))) / (double)(((double)(maxd_1) - (double)(mind_1)))))))));
                    }
                    j_3 = (long)((long)(j_3) + 1L);
                }
            } else {
                while ((long)(j_3) < (long)(dlist_1.length)) {
                    double item_3 = (double)(dlist_1[(int)((long)(j_3))]);
                    if ((double)((double)(maxd_1) - (double)(mind_1)) == (double)(0.0)) {
                        score_1 = ((double[])(appendDouble(score_1, (double)(0.0))));
                    } else {
                        score_1 = ((double[])(appendDouble(score_1, (double)((double)(((double)(item_3) - (double)(mind_1))) / (double)(((double)(maxd_1) - (double)(mind_1)))))));
                    }
                    j_3 = (long)((long)(j_3) + 1L);
                }
            }
            score_lists = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(score_lists), java.util.stream.Stream.of(new double[][]{score_1})).toArray(double[][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return score_lists;
    }

    static double[] generate_final_scores(double[][] score_lists) {
        long count = (long)(score_lists[(int)((long)(0))].length);
        double[] final_scores_1 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(count)) {
            final_scores_1 = ((double[])(appendDouble(final_scores_1, (double)(0.0))));
            i_5 = (long)((long)(i_5) + 1L);
        }
        i_5 = 0L;
        while ((long)(i_5) < (long)(score_lists.length)) {
            double[] slist_1 = ((double[])(score_lists[(int)((long)(i_5))]));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(slist_1.length)) {
final_scores_1[(int)((long)(j_5))] = (double)((double)(final_scores_1[(int)((long)(j_5))]) + (double)(slist_1[(int)((long)(j_5))]));
                j_5 = (long)((long)(j_5) + 1L);
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        return final_scores_1;
    }

    static double[][] procentual_proximity(double[][] source_data, long[] weights) {
        double[][] data_lists_1 = ((double[][])(get_data(((double[][])(source_data)))));
        double[][] score_lists_2 = ((double[][])(calculate_each_score(((double[][])(data_lists_1)), ((long[])(weights)))));
        double[] final_scores_3 = ((double[])(generate_final_scores(((double[][])(score_lists_2)))));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(final_scores_3.length)) {
source_data[(int)((long)(i_7))] = ((double[])(appendDouble(source_data[(int)((long)(i_7))], (double)(final_scores_3[(int)((long)(i_7))]))));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return source_data;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            vehicles = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vehicles), java.util.stream.Stream.of(new double[][]{new double[]{20.0, 60.0, 2012.0}})).toArray(double[][]::new)));
            vehicles = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vehicles), java.util.stream.Stream.of(new double[][]{new double[]{23.0, 90.0, 2015.0}})).toArray(double[][]::new)));
            vehicles = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vehicles), java.util.stream.Stream.of(new double[][]{new double[]{22.0, 50.0, 2011.0}})).toArray(double[][]::new)));
            result = ((double[][])(procentual_proximity(((double[][])(vehicles)), ((long[])(weights)))));
            System.out.println(_p(result));
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
