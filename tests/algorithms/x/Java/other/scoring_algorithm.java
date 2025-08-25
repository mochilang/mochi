public class Main {
    static double[][] vehicles = ((double[][])(new double[][]{}));
    static java.math.BigInteger[] weights = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1)}));
    static double[][] result;

    static double[][] get_data(double[][] source_data) {
        double[][] data_lists = ((double[][])(new double[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(source_data.length))) < 0) {
            double[] row_1 = ((double[])(source_data[_idx((source_data).length, ((java.math.BigInteger)(i_1)).longValue())]));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(row_1.length))) < 0) {
                if (new java.math.BigInteger(String.valueOf(data_lists.length)).compareTo(j_1.add(java.math.BigInteger.valueOf(1))) < 0) {
                    double[] empty_1 = ((double[])(new double[]{}));
                    data_lists = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(data_lists), java.util.stream.Stream.of(new double[][]{((double[])(empty_1))})).toArray(double[][]::new)));
                }
data_lists[(int)(((java.math.BigInteger)(j_1)).longValue())] = ((double[])(appendDouble(data_lists[_idx((data_lists).length, ((java.math.BigInteger)(j_1)).longValue())], (double)(row_1[_idx((row_1).length, ((java.math.BigInteger)(j_1)).longValue())]))));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[][])(data_lists));
    }

    static double[][] calculate_each_score(double[][] data_lists, java.math.BigInteger[] weights) {
        double[][] score_lists = ((double[][])(new double[][]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(data_lists.length))) < 0) {
            double[] dlist_1 = ((double[])(data_lists[_idx((data_lists).length, ((java.math.BigInteger)(i_3)).longValue())]));
            java.math.BigInteger weight_1 = new java.math.BigInteger(String.valueOf(weights[_idx((weights).length, ((java.math.BigInteger)(i_3)).longValue())]));
            double mind_1 = (double)(dlist_1[_idx((dlist_1).length, 0L)]);
            double maxd_1 = (double)(dlist_1[_idx((dlist_1).length, 0L)]);
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(1);
            while (j_3.compareTo(new java.math.BigInteger(String.valueOf(dlist_1.length))) < 0) {
                double val_1 = (double)(dlist_1[_idx((dlist_1).length, ((java.math.BigInteger)(j_3)).longValue())]);
                if ((double)(val_1) < (double)(mind_1)) {
                    mind_1 = (double)(val_1);
                }
                if ((double)(val_1) > (double)(maxd_1)) {
                    maxd_1 = (double)(val_1);
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            double[] score_1 = ((double[])(new double[]{}));
            j_3 = java.math.BigInteger.valueOf(0);
            if (weight_1.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                while (j_3.compareTo(new java.math.BigInteger(String.valueOf(dlist_1.length))) < 0) {
                    double item_2 = (double)(dlist_1[_idx((dlist_1).length, ((java.math.BigInteger)(j_3)).longValue())]);
                    if ((double)((double)(maxd_1) - (double)(mind_1)) == (double)(0.0)) {
                        score_1 = ((double[])(appendDouble(score_1, (double)(1.0))));
                    } else {
                        score_1 = ((double[])(appendDouble(score_1, (double)((double)(1.0) - (double)(((double)(((double)(item_2) - (double)(mind_1))) / (double)(((double)(maxd_1) - (double)(mind_1)))))))));
                    }
                    j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
                }
            } else {
                while (j_3.compareTo(new java.math.BigInteger(String.valueOf(dlist_1.length))) < 0) {
                    double item_3 = (double)(dlist_1[_idx((dlist_1).length, ((java.math.BigInteger)(j_3)).longValue())]);
                    if ((double)((double)(maxd_1) - (double)(mind_1)) == (double)(0.0)) {
                        score_1 = ((double[])(appendDouble(score_1, (double)(0.0))));
                    } else {
                        score_1 = ((double[])(appendDouble(score_1, (double)((double)(((double)(item_3) - (double)(mind_1))) / (double)(((double)(maxd_1) - (double)(mind_1)))))));
                    }
                    j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
                }
            }
            score_lists = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(score_lists), java.util.stream.Stream.of(new double[][]{((double[])(score_1))})).toArray(double[][]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[][])(score_lists));
    }

    static double[] generate_final_scores(double[][] score_lists) {
        java.math.BigInteger count = new java.math.BigInteger(String.valueOf(score_lists[_idx((score_lists).length, 0L)].length));
        double[] final_scores_1 = ((double[])(new double[]{}));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(count) < 0) {
            final_scores_1 = ((double[])(appendDouble(final_scores_1, (double)(0.0))));
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(score_lists.length))) < 0) {
            double[] slist_1 = ((double[])(score_lists[_idx((score_lists).length, ((java.math.BigInteger)(i_5)).longValue())]));
            java.math.BigInteger j_5 = java.math.BigInteger.valueOf(0);
            while (j_5.compareTo(new java.math.BigInteger(String.valueOf(slist_1.length))) < 0) {
final_scores_1[(int)(((java.math.BigInteger)(j_5)).longValue())] = (double)((double)(final_scores_1[_idx((final_scores_1).length, ((java.math.BigInteger)(j_5)).longValue())]) + (double)(slist_1[_idx((slist_1).length, ((java.math.BigInteger)(j_5)).longValue())]));
                j_5 = new java.math.BigInteger(String.valueOf(j_5.add(java.math.BigInteger.valueOf(1))));
            }
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(final_scores_1));
    }

    static double[][] procentual_proximity(double[][] source_data, java.math.BigInteger[] weights) {
        double[][] data_lists_1 = ((double[][])(get_data(((double[][])(source_data)))));
        double[][] score_lists_2 = ((double[][])(calculate_each_score(((double[][])(data_lists_1)), ((java.math.BigInteger[])(weights)))));
        double[] final_scores_3 = ((double[])(generate_final_scores(((double[][])(score_lists_2)))));
        java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(final_scores_3.length))) < 0) {
source_data[(int)(((java.math.BigInteger)(i_7)).longValue())] = ((double[])(appendDouble(source_data[_idx((source_data).length, ((java.math.BigInteger)(i_7)).longValue())], (double)(final_scores_3[_idx((final_scores_3).length, ((java.math.BigInteger)(i_7)).longValue())]))));
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[][])(source_data));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            vehicles = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vehicles), java.util.stream.Stream.of(new double[][]{((double[])(new double[]{(double)(20.0), (double)(60.0), (double)(2012.0)}))})).toArray(double[][]::new)));
            vehicles = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vehicles), java.util.stream.Stream.of(new double[][]{((double[])(new double[]{(double)(23.0), (double)(90.0), (double)(2015.0)}))})).toArray(double[][]::new)));
            vehicles = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vehicles), java.util.stream.Stream.of(new double[][]{((double[])(new double[]{(double)(22.0), (double)(50.0), (double)(2011.0)}))})).toArray(double[][]::new)));
            result = ((double[][])(procentual_proximity(((double[][])(vehicles)), ((java.math.BigInteger[])(weights)))));
            System.out.println(_p(result));
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
