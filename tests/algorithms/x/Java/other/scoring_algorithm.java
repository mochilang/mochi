public class Main {
    static double[][] vehicles = new double[0][];
    static int[] weights = new int[0];
    static double[][] result;

    static double[][] get_data(double[][] source_data) {
        double[][] data_lists = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < source_data.length) {
            double[] row = ((double[])(source_data[i]));
            int j = 0;
            while (j < row.length) {
                if (data_lists.length < j + 1) {
                    double[] empty = ((double[])(new double[]{}));
                    data_lists = ((double[][])(appendObj(data_lists, empty)));
                }
data_lists[j] = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(data_lists[j]), java.util.stream.DoubleStream.of(row[j])).toArray()));
                j = j + 1;
            }
            i = i + 1;
        }
        return data_lists;
    }

    static double[][] calculate_each_score(double[][] data_lists, int[] weights) {
        double[][] score_lists = ((double[][])(new double[][]{}));
        int i_1 = 0;
        while (i_1 < data_lists.length) {
            double[] dlist = ((double[])(data_lists[i_1]));
            int weight = weights[i_1];
            double mind = dlist[0];
            double maxd = dlist[0];
            int j_1 = 1;
            while (j_1 < dlist.length) {
                double val = dlist[j_1];
                if (val < mind) {
                    mind = val;
                }
                if (val > maxd) {
                    maxd = val;
                }
                j_1 = j_1 + 1;
            }
            double[] score = ((double[])(new double[]{}));
            j_1 = 0;
            if (weight == 0) {
                while (j_1 < dlist.length) {
                    double item = dlist[j_1];
                    if (maxd - mind == 0.0) {
                        score = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(score), java.util.stream.DoubleStream.of(1.0)).toArray()));
                    } else {
                        score = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(score), java.util.stream.DoubleStream.of(1.0 - ((item - mind) / (maxd - mind)))).toArray()));
                    }
                    j_1 = j_1 + 1;
                }
            } else {
                while (j_1 < dlist.length) {
                    double item_1 = dlist[j_1];
                    if (maxd - mind == 0.0) {
                        score = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(score), java.util.stream.DoubleStream.of(0.0)).toArray()));
                    } else {
                        score = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(score), java.util.stream.DoubleStream.of((item_1 - mind) / (maxd - mind))).toArray()));
                    }
                    j_1 = j_1 + 1;
                }
            }
            score_lists = ((double[][])(appendObj(score_lists, score)));
            i_1 = i_1 + 1;
        }
        return score_lists;
    }

    static double[] generate_final_scores(double[][] score_lists) {
        int count = score_lists[0].length;
        double[] final_scores = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < count) {
            final_scores = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(final_scores), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_2 = i_2 + 1;
        }
        i_2 = 0;
        while (i_2 < score_lists.length) {
            double[] slist = ((double[])(score_lists[i_2]));
            int j_2 = 0;
            while (j_2 < slist.length) {
final_scores[j_2] = final_scores[j_2] + slist[j_2];
                j_2 = j_2 + 1;
            }
            i_2 = i_2 + 1;
        }
        return final_scores;
    }

    static double[][] procentual_proximity(double[][] source_data, int[] weights) {
        double[][] data_lists_1 = ((double[][])(get_data(((double[][])(source_data)))));
        double[][] score_lists_1 = ((double[][])(calculate_each_score(((double[][])(data_lists_1)), ((int[])(weights)))));
        double[] final_scores_1 = ((double[])(generate_final_scores(((double[][])(score_lists_1)))));
        int i_3 = 0;
        while (i_3 < final_scores_1.length) {
source_data[i_3] = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(source_data[i_3]), java.util.stream.DoubleStream.of(final_scores_1[i_3])).toArray()));
            i_3 = i_3 + 1;
        }
        return source_data;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            vehicles = ((double[][])(new double[][]{}));
            vehicles = ((double[][])(appendObj(vehicles, new double[]{20.0, 60.0, 2012.0})));
            vehicles = ((double[][])(appendObj(vehicles, new double[]{23.0, 90.0, 2015.0})));
            vehicles = ((double[][])(appendObj(vehicles, new double[]{22.0, 50.0, 2011.0})));
            weights = ((int[])(new int[]{0, 0, 1}));
            result = ((double[][])(procentual_proximity(((double[][])(vehicles)), ((int[])(weights)))));
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
}
