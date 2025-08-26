public class Main {
    static double c = (double)(299792458.0);
    static double[] v;

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return (double)(0.0);
        }
        double guess_1 = (double)((double)(x) / (double)(2.0));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(guess_1);
    }

    static double beta(double velocity) {
        if ((double)(velocity) > (double)(c)) {
            throw new RuntimeException(String.valueOf("Speed must not exceed light speed 299,792,458 [m/s]!"));
        }
        if ((double)(velocity) < (double)(1.0)) {
            throw new RuntimeException(String.valueOf("Speed must be greater than or equal to 1!"));
        }
        return (double)((double)(velocity) / (double)(c));
    }

    static double gamma(double velocity) {
        double b = (double)(beta((double)(velocity)));
        return (double)((double)(1.0) / (double)(sqrtApprox((double)((double)(1.0) - (double)((double)(b) * (double)(b))))));
    }

    static double[][] transformation_matrix(double velocity) {
        double g = (double)(gamma((double)(velocity)));
        double b_2 = (double)(beta((double)(velocity)));
        return ((double[][])(new double[][]{((double[])(new double[]{(double)(g), (double)((double)(-g) * (double)(b_2)), (double)(0.0), (double)(0.0)})), ((double[])(new double[]{(double)((double)(-g) * (double)(b_2)), (double)(g), (double)(0.0), (double)(0.0)})), ((double[])(new double[]{(double)(0.0), (double)(0.0), (double)(1.0), (double)(0.0)})), ((double[])(new double[]{(double)(0.0), (double)(0.0), (double)(0.0), (double)(1.0)}))}));
    }

    static double[] mat_vec_mul(double[][] mat, double[] vec) {
        double[] res = ((double[])(new double[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
            double[] row_1 = ((double[])(mat[_idx((mat).length, ((java.math.BigInteger)(i_3)).longValue())]));
            double value_1 = (double)((double)((double)((double)((double)(row_1[_idx((row_1).length, 0L)]) * (double)(vec[_idx((vec).length, 0L)])) + (double)((double)(row_1[_idx((row_1).length, 1L)]) * (double)(vec[_idx((vec).length, 1L)]))) + (double)((double)(row_1[_idx((row_1).length, 2L)]) * (double)(vec[_idx((vec).length, 2L)]))) + (double)((double)(row_1[_idx((row_1).length, 3L)]) * (double)(vec[_idx((vec).length, 3L)])));
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new double[]{(double)(value_1)})).toArray()));
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        return ((double[])(res));
    }

    static double[] transform(double velocity, double[] event) {
        double g_1 = (double)(gamma((double)(velocity)));
        double b_4 = (double)(beta((double)(velocity)));
        double ct_1 = (double)((double)(event[_idx((event).length, 0L)]) * (double)(c));
        double x_1 = (double)(event[_idx((event).length, 1L)]);
        return ((double[])(new double[]{(double)((double)((double)(g_1) * (double)(ct_1)) - (double)((double)((double)(g_1) * (double)(b_4)) * (double)(x_1))), (double)((double)((double)((double)(-g_1) * (double)(b_4)) * (double)(ct_1)) + (double)((double)(g_1) * (double)(x_1))), (double)(event[_idx((event).length, 2L)]), (double)(event[_idx((event).length, 3L)])}));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(beta((double)(c))));
            System.out.println(_p(beta((double)(199792458.0))));
            System.out.println(_p(beta((double)(100000.0))));
            System.out.println(_p(gamma((double)(4.0))));
            System.out.println(_p(gamma((double)(100000.0))));
            System.out.println(_p(gamma((double)(30000000.0))));
            System.out.println(_p(transformation_matrix((double)(29979245.0))));
            v = ((double[])(transform((double)(29979245.0), ((double[])(new double[]{(double)(1.0), (double)(2.0), (double)(3.0), (double)(4.0)})))));
            System.out.println(_p(v));
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
