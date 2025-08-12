public class Main {
    static class Network {
        double[][] w1;
        double[][] w2;
        double[][] w3;
        Network(double[][] w1, double[][] w2, double[][] w3) {
            this.w1 = w1;
            this.w2 = w2;
            this.w3 = w3;
        }
        Network() {}
        @Override public String toString() {
            return String.format("{'w1': %s, 'w2': %s, 'w3': %s}", String.valueOf(w1), String.valueOf(w2), String.valueOf(w3));
        }
    }


    static double exp_approx(double x) {
        double sum = 1.0;
        double term_1 = 1.0;
        long i_1 = 1L;
        while (i_1 < (long)(10)) {
            term_1 = (double)(term_1) * (double)(x) / ((Number)(i_1)).doubleValue();
            sum = (double)(sum) + (double)(term_1);
            i_1 = (long)(i_1 + (long)(1));
        }
        return sum;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + (double)(exp_approx((double)(-x))));
    }

    static double sigmoid_derivative(double x) {
        return (double)(x) * (1.0 - (double)(x));
    }

    static Network new_network() {
        return new Network(new double[][]{new double[]{0.1, 0.2, 0.3, 0.4}, new double[]{0.5, 0.6, 0.7, 0.8}, new double[]{0.9, 1.0, 1.1, 1.2}}, new double[][]{new double[]{0.1, 0.2, 0.3}, new double[]{0.4, 0.5, 0.6}, new double[]{0.7, 0.8, 0.9}, new double[]{1.0, 1.1, 1.2}}, new double[][]{new double[]{0.1}, new double[]{0.2}, new double[]{0.3}});
    }

    static double feedforward(Network net, double[] input) {
        double[] hidden1 = ((double[])(new double[]{}));
        long j_1 = 0L;
        while (j_1 < (long)(4)) {
            double sum1_1 = 0.0;
            long i_3 = 0L;
            while (i_3 < (long)(3)) {
                sum1_1 = (double)(sum1_1) + (double)(input[(int)((long)(i_3))]) * net.w1[(int)((long)(i_3))][(int)((long)(j_1))];
                i_3 = (long)(i_3 + (long)(1));
            }
            hidden1 = ((double[])(appendDouble(hidden1, (double)(sigmoid((double)(sum1_1))))));
            j_1 = (long)(j_1 + (long)(1));
        }
        double[] hidden2_1 = ((double[])(new double[]{}));
        long k_1 = 0L;
        while (k_1 < (long)(3)) {
            double sum2_1 = 0.0;
            long j2_1 = 0L;
            while (j2_1 < (long)(4)) {
                sum2_1 = (double)(sum2_1) + (double)(hidden1[(int)((long)(j2_1))]) * net.w2[(int)((long)(j2_1))][(int)((long)(k_1))];
                j2_1 = (long)(j2_1 + (long)(1));
            }
            hidden2_1 = ((double[])(appendDouble(hidden2_1, (double)(sigmoid((double)(sum2_1))))));
            k_1 = (long)(k_1 + (long)(1));
        }
        double sum3_1 = 0.0;
        long k2_1 = 0L;
        while (k2_1 < (long)(3)) {
            sum3_1 = (double)(sum3_1) + (double)(hidden2_1[(int)((long)(k2_1))]) * net.w3[(int)((long)(k2_1))][(int)((long)(0))];
            k2_1 = (long)(k2_1 + (long)(1));
        }
        double out_1 = (double)(sigmoid((double)(sum3_1)));
        return out_1;
    }

    static void train(Network net, double[][] inputs, double[] outputs, long iterations) {
        long iter = 0L;
        while (iter < iterations) {
            long s_1 = 0L;
            while (s_1 < (long)(inputs.length)) {
                double[] inp_1 = ((double[])(inputs[(int)((long)(s_1))]));
                double target_1 = (double)(outputs[(int)((long)(s_1))]);
                double[] hidden1_2 = ((double[])(new double[]{}));
                long j_3 = 0L;
                while (j_3 < (long)(4)) {
                    double sum1_3 = 0.0;
                    long i_5 = 0L;
                    while (i_5 < (long)(3)) {
                        sum1_3 = (double)(sum1_3) + (double)(inp_1[(int)((long)(i_5))]) * net.w1[(int)((long)(i_5))][(int)((long)(j_3))];
                        i_5 = (long)(i_5 + (long)(1));
                    }
                    hidden1_2 = ((double[])(appendDouble(hidden1_2, (double)(sigmoid((double)(sum1_3))))));
                    j_3 = (long)(j_3 + (long)(1));
                }
                double[] hidden2_3 = ((double[])(new double[]{}));
                long k_3 = 0L;
                while (k_3 < (long)(3)) {
                    double sum2_3 = 0.0;
                    long j2_3 = 0L;
                    while (j2_3 < (long)(4)) {
                        sum2_3 = (double)(sum2_3) + (double)(hidden1_2[(int)((long)(j2_3))]) * net.w2[(int)((long)(j2_3))][(int)((long)(k_3))];
                        j2_3 = (long)(j2_3 + (long)(1));
                    }
                    hidden2_3 = ((double[])(appendDouble(hidden2_3, (double)(sigmoid((double)(sum2_3))))));
                    k_3 = (long)(k_3 + (long)(1));
                }
                double sum3_3 = 0.0;
                long k3_1 = 0L;
                while (k3_1 < (long)(3)) {
                    sum3_3 = (double)(sum3_3) + (double)(hidden2_3[(int)((long)(k3_1))]) * net.w3[(int)((long)(k3_1))][(int)((long)(0))];
                    k3_1 = (long)(k3_1 + (long)(1));
                }
                double output_1 = (double)(sigmoid((double)(sum3_3)));
                double error_1 = (double)(target_1) - (double)(output_1);
                double delta_output_1 = error_1 * (double)(sigmoid_derivative((double)(output_1)));
                double[][] new_w3_1 = ((double[][])(new double[][]{}));
                long k4_1 = 0L;
                while (k4_1 < (long)(3)) {
                    double[] w3row_1 = ((double[])(((double[])(net.w3[(int)((long)(k4_1))]))));
w3row_1[(int)((long)(0))] = (double)(w3row_1[(int)((long)(0))]) + (double)(hidden2_3[(int)((long)(k4_1))]) * delta_output_1;
                    new_w3_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_w3_1), java.util.stream.Stream.of(w3row_1)).toArray(double[][]::new)));
                    k4_1 = (long)(k4_1 + (long)(1));
                }
net.w3 = new_w3_1;
                double[] delta_hidden2_1 = ((double[])(new double[]{}));
                long k5_1 = 0L;
                while (k5_1 < (long)(3)) {
                    double[] row_1 = ((double[])(net.w3[(int)((long)(k5_1))]));
                    double dh2_1 = row_1[(int)((long)(0))] * delta_output_1 * (double)(sigmoid_derivative((double)(hidden2_3[(int)((long)(k5_1))])));
                    delta_hidden2_1 = ((double[])(appendDouble(delta_hidden2_1, dh2_1)));
                    k5_1 = (long)(k5_1 + (long)(1));
                }
                double[][] new_w2_1 = ((double[][])(new double[][]{}));
                j_3 = 0L;
                while (j_3 < (long)(4)) {
                    double[] w2row_1 = ((double[])(((double[])(net.w2[(int)((long)(j_3))]))));
                    long k6_1 = 0L;
                    while (k6_1 < (long)(3)) {
w2row_1[(int)((long)(k6_1))] = (double)(w2row_1[(int)((long)(k6_1))]) + (double)(hidden1_2[(int)((long)(j_3))]) * (double)(delta_hidden2_1[(int)((long)(k6_1))]);
                        k6_1 = (long)(k6_1 + (long)(1));
                    }
                    new_w2_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_w2_1), java.util.stream.Stream.of(w2row_1)).toArray(double[][]::new)));
                    j_3 = (long)(j_3 + (long)(1));
                }
net.w2 = new_w2_1;
                double[] delta_hidden1_1 = ((double[])(new double[]{}));
                j_3 = 0L;
                while (j_3 < (long)(4)) {
                    double sumdh_1 = 0.0;
                    long k7_1 = 0L;
                    while (k7_1 < (long)(3)) {
                        double[] row2_1 = ((double[])(net.w2[(int)((long)(j_3))]));
                        sumdh_1 = (double)(sumdh_1) + row2_1[(int)((long)(k7_1))] * (double)(delta_hidden2_1[(int)((long)(k7_1))]);
                        k7_1 = (long)(k7_1 + (long)(1));
                    }
                    delta_hidden1_1 = ((double[])(appendDouble(delta_hidden1_1, (double)(sumdh_1) * (double)(sigmoid_derivative((double)(hidden1_2[(int)((long)(j_3))]))))));
                    j_3 = (long)(j_3 + (long)(1));
                }
                double[][] new_w1_1 = ((double[][])(new double[][]{}));
                long i2_1 = 0L;
                while (i2_1 < (long)(3)) {
                    double[] w1row_1 = ((double[])(((double[])(net.w1[(int)((long)(i2_1))]))));
                    j_3 = 0L;
                    while (j_3 < (long)(4)) {
w1row_1[(int)((long)(j_3))] = (double)(w1row_1[(int)((long)(j_3))]) + (double)(inp_1[(int)((long)(i2_1))]) * (double)(delta_hidden1_1[(int)((long)(j_3))]);
                        j_3 = (long)(j_3 + (long)(1));
                    }
                    new_w1_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_w1_1), java.util.stream.Stream.of(w1row_1)).toArray(double[][]::new)));
                    i2_1 = (long)(i2_1 + (long)(1));
                }
net.w1 = new_w1_1;
                s_1 = (long)(s_1 + (long)(1));
            }
            iter = (long)(iter + (long)(1));
        }
    }

    static long predict(Network net, double[] input) {
        double out_2 = (double)(feedforward(net, ((double[])(input))));
        if ((double)(out_2) > 0.6) {
            return 1;
        }
        return 0;
    }

    static long example() {
        double[][] inputs = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0}, new double[]{0.0, 1.0, 0.0}, new double[]{0.0, 1.0, 1.0}, new double[]{1.0, 0.0, 0.0}, new double[]{1.0, 0.0, 1.0}, new double[]{1.0, 1.0, 0.0}, new double[]{1.0, 1.0, 1.0}}));
        double[] outputs_1 = ((double[])(new double[]{0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0}));
        Network net_1 = new_network();
        train(net_1, ((double[][])(inputs)), ((double[])(outputs_1)), 10L);
        long result_1 = predict(net_1, ((double[])(new double[]{1.0, 1.0, 1.0})));
        System.out.println(_p(result_1));
        return result_1;
    }

    static void main() {
        example();
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
