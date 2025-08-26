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
        double sum = (double)(1.0);
        double term_1 = (double)(1.0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(java.math.BigInteger.valueOf(10)) < 0) {
            term_1 = (double)((double)((double)(term_1) * (double)(x)) / (double)(((Number)(i_1)).doubleValue()));
            sum = (double)((double)(sum) + (double)(term_1));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(sum);
    }

    static double sigmoid(double x) {
        return (double)((double)(1.0) / (double)(((double)(1.0) + (double)(exp_approx((double)(-x))))));
    }

    static double sigmoid_derivative(double x) {
        return (double)((double)(x) * (double)(((double)(1.0) - (double)(x))));
    }

    static Network new_network() {
        return new Network(((double[][])(new double[][]{((double[])(new double[]{(double)(0.1), (double)(0.2), (double)(0.3), (double)(0.4)})), ((double[])(new double[]{(double)(0.5), (double)(0.6), (double)(0.7), (double)(0.8)})), ((double[])(new double[]{(double)(0.9), (double)(1.0), (double)(1.1), (double)(1.2)}))})), ((double[][])(new double[][]{((double[])(new double[]{(double)(0.1), (double)(0.2), (double)(0.3)})), ((double[])(new double[]{(double)(0.4), (double)(0.5), (double)(0.6)})), ((double[])(new double[]{(double)(0.7), (double)(0.8), (double)(0.9)})), ((double[])(new double[]{(double)(1.0), (double)(1.1), (double)(1.2)}))})), ((double[][])(new double[][]{((double[])(new double[]{(double)(0.1)})), ((double[])(new double[]{(double)(0.2)})), ((double[])(new double[]{(double)(0.3)}))})));
    }

    static double feedforward(Network net, double[] input) {
        double[] hidden1 = ((double[])(new double[]{}));
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
        while (j_1.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
            double sum1_1 = (double)(0.0);
            java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
            while (i_3.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
                sum1_1 = (double)((double)(sum1_1) + (double)((double)(input[_idx((input).length, ((java.math.BigInteger)(i_3)).longValue())]) * (double)(net.w1[_idx((net.w1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((net.w1[_idx((net.w1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_1)).longValue())])));
                i_3 = i_3.add(java.math.BigInteger.valueOf(1));
            }
            hidden1 = ((double[])(appendDouble(hidden1, (double)(sigmoid((double)(sum1_1))))));
            j_1 = j_1.add(java.math.BigInteger.valueOf(1));
        }
        double[] hidden2_1 = ((double[])(new double[]{}));
        java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
        while (k_1.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
            double sum2_1 = (double)(0.0);
            java.math.BigInteger j2_1 = java.math.BigInteger.valueOf(0);
            while (j2_1.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
                sum2_1 = (double)((double)(sum2_1) + (double)((double)(hidden1[_idx((hidden1).length, ((java.math.BigInteger)(j2_1)).longValue())]) * (double)(net.w2[_idx((net.w2).length, ((java.math.BigInteger)(j2_1)).longValue())][_idx((net.w2[_idx((net.w2).length, ((java.math.BigInteger)(j2_1)).longValue())]).length, ((java.math.BigInteger)(k_1)).longValue())])));
                j2_1 = j2_1.add(java.math.BigInteger.valueOf(1));
            }
            hidden2_1 = ((double[])(appendDouble(hidden2_1, (double)(sigmoid((double)(sum2_1))))));
            k_1 = k_1.add(java.math.BigInteger.valueOf(1));
        }
        double sum3_1 = (double)(0.0);
        java.math.BigInteger k2_1 = java.math.BigInteger.valueOf(0);
        while (k2_1.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
            sum3_1 = (double)((double)(sum3_1) + (double)((double)(hidden2_1[_idx((hidden2_1).length, ((java.math.BigInteger)(k2_1)).longValue())]) * (double)(net.w3[_idx((net.w3).length, ((java.math.BigInteger)(k2_1)).longValue())][_idx((net.w3[_idx((net.w3).length, ((java.math.BigInteger)(k2_1)).longValue())]).length, 0L)])));
            k2_1 = k2_1.add(java.math.BigInteger.valueOf(1));
        }
        double out_1 = (double)(sigmoid((double)(sum3_1)));
        return (double)(out_1);
    }

    static void train(Network net, double[][] inputs, double[] outputs, java.math.BigInteger iterations) {
        java.math.BigInteger iter = java.math.BigInteger.valueOf(0);
        while (iter.compareTo(iterations) < 0) {
            java.math.BigInteger s_1 = java.math.BigInteger.valueOf(0);
            while (s_1.compareTo(new java.math.BigInteger(String.valueOf(inputs.length))) < 0) {
                double[] inp_1 = ((double[])(inputs[_idx((inputs).length, ((java.math.BigInteger)(s_1)).longValue())]));
                double target_1 = (double)(outputs[_idx((outputs).length, ((java.math.BigInteger)(s_1)).longValue())]);
                double[] hidden1_2 = ((double[])(new double[]{}));
                java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
                while (j_3.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
                    double sum1_3 = (double)(0.0);
                    java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
                    while (i_5.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
                        sum1_3 = (double)((double)(sum1_3) + (double)((double)(inp_1[_idx((inp_1).length, ((java.math.BigInteger)(i_5)).longValue())]) * (double)(net.w1[_idx((net.w1).length, ((java.math.BigInteger)(i_5)).longValue())][_idx((net.w1[_idx((net.w1).length, ((java.math.BigInteger)(i_5)).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())])));
                        i_5 = i_5.add(java.math.BigInteger.valueOf(1));
                    }
                    hidden1_2 = ((double[])(appendDouble(hidden1_2, (double)(sigmoid((double)(sum1_3))))));
                    j_3 = j_3.add(java.math.BigInteger.valueOf(1));
                }
                double[] hidden2_3 = ((double[])(new double[]{}));
                java.math.BigInteger k_3 = java.math.BigInteger.valueOf(0);
                while (k_3.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
                    double sum2_3 = (double)(0.0);
                    java.math.BigInteger j2_3 = java.math.BigInteger.valueOf(0);
                    while (j2_3.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
                        sum2_3 = (double)((double)(sum2_3) + (double)((double)(hidden1_2[_idx((hidden1_2).length, ((java.math.BigInteger)(j2_3)).longValue())]) * (double)(net.w2[_idx((net.w2).length, ((java.math.BigInteger)(j2_3)).longValue())][_idx((net.w2[_idx((net.w2).length, ((java.math.BigInteger)(j2_3)).longValue())]).length, ((java.math.BigInteger)(k_3)).longValue())])));
                        j2_3 = j2_3.add(java.math.BigInteger.valueOf(1));
                    }
                    hidden2_3 = ((double[])(appendDouble(hidden2_3, (double)(sigmoid((double)(sum2_3))))));
                    k_3 = k_3.add(java.math.BigInteger.valueOf(1));
                }
                double sum3_3 = (double)(0.0);
                java.math.BigInteger k3_1 = java.math.BigInteger.valueOf(0);
                while (k3_1.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
                    sum3_3 = (double)((double)(sum3_3) + (double)((double)(hidden2_3[_idx((hidden2_3).length, ((java.math.BigInteger)(k3_1)).longValue())]) * (double)(net.w3[_idx((net.w3).length, ((java.math.BigInteger)(k3_1)).longValue())][_idx((net.w3[_idx((net.w3).length, ((java.math.BigInteger)(k3_1)).longValue())]).length, 0L)])));
                    k3_1 = k3_1.add(java.math.BigInteger.valueOf(1));
                }
                double output_1 = (double)(sigmoid((double)(sum3_3)));
                double error_1 = (double)((double)(target_1) - (double)(output_1));
                double delta_output_1 = (double)((double)(error_1) * (double)(sigmoid_derivative((double)(output_1))));
                double[][] new_w3_1 = ((double[][])(new double[][]{}));
                java.math.BigInteger k4_1 = java.math.BigInteger.valueOf(0);
                while (k4_1.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
                    double[] w3row_1 = ((double[])(((double[])(net.w3[_idx((net.w3).length, ((java.math.BigInteger)(k4_1)).longValue())]))));
w3row_1[(int)(0L)] = (double)((double)(w3row_1[_idx((w3row_1).length, 0L)]) + (double)((double)(hidden2_3[_idx((hidden2_3).length, ((java.math.BigInteger)(k4_1)).longValue())]) * (double)(delta_output_1)));
                    new_w3_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_w3_1), java.util.stream.Stream.of(new double[][]{((double[])(w3row_1))})).toArray(double[][]::new)));
                    k4_1 = k4_1.add(java.math.BigInteger.valueOf(1));
                }
net.w3 = new_w3_1;
                double[] delta_hidden2_1 = ((double[])(new double[]{}));
                java.math.BigInteger k5_1 = java.math.BigInteger.valueOf(0);
                while (k5_1.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
                    double[] row_1 = ((double[])(net.w3[_idx((net.w3).length, ((java.math.BigInteger)(k5_1)).longValue())]));
                    double dh2_1 = (double)((double)((double)(row_1[_idx((row_1).length, 0L)]) * (double)(delta_output_1)) * (double)(sigmoid_derivative((double)(hidden2_3[_idx((hidden2_3).length, ((java.math.BigInteger)(k5_1)).longValue())]))));
                    delta_hidden2_1 = ((double[])(appendDouble(delta_hidden2_1, (double)(dh2_1))));
                    k5_1 = k5_1.add(java.math.BigInteger.valueOf(1));
                }
                double[][] new_w2_1 = ((double[][])(new double[][]{}));
                j_3 = java.math.BigInteger.valueOf(0);
                while (j_3.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
                    double[] w2row_1 = ((double[])(((double[])(net.w2[_idx((net.w2).length, ((java.math.BigInteger)(j_3)).longValue())]))));
                    java.math.BigInteger k6_1 = java.math.BigInteger.valueOf(0);
                    while (k6_1.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
w2row_1[(int)(((java.math.BigInteger)(k6_1)).longValue())] = (double)((double)(w2row_1[_idx((w2row_1).length, ((java.math.BigInteger)(k6_1)).longValue())]) + (double)((double)(hidden1_2[_idx((hidden1_2).length, ((java.math.BigInteger)(j_3)).longValue())]) * (double)(delta_hidden2_1[_idx((delta_hidden2_1).length, ((java.math.BigInteger)(k6_1)).longValue())])));
                        k6_1 = k6_1.add(java.math.BigInteger.valueOf(1));
                    }
                    new_w2_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_w2_1), java.util.stream.Stream.of(new double[][]{((double[])(w2row_1))})).toArray(double[][]::new)));
                    j_3 = j_3.add(java.math.BigInteger.valueOf(1));
                }
net.w2 = new_w2_1;
                double[] delta_hidden1_1 = ((double[])(new double[]{}));
                j_3 = java.math.BigInteger.valueOf(0);
                while (j_3.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
                    double sumdh_1 = (double)(0.0);
                    java.math.BigInteger k7_1 = java.math.BigInteger.valueOf(0);
                    while (k7_1.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
                        double[] row2_1 = ((double[])(net.w2[_idx((net.w2).length, ((java.math.BigInteger)(j_3)).longValue())]));
                        sumdh_1 = (double)((double)(sumdh_1) + (double)((double)(row2_1[_idx((row2_1).length, ((java.math.BigInteger)(k7_1)).longValue())]) * (double)(delta_hidden2_1[_idx((delta_hidden2_1).length, ((java.math.BigInteger)(k7_1)).longValue())])));
                        k7_1 = k7_1.add(java.math.BigInteger.valueOf(1));
                    }
                    delta_hidden1_1 = ((double[])(appendDouble(delta_hidden1_1, (double)((double)(sumdh_1) * (double)(sigmoid_derivative((double)(hidden1_2[_idx((hidden1_2).length, ((java.math.BigInteger)(j_3)).longValue())])))))));
                    j_3 = j_3.add(java.math.BigInteger.valueOf(1));
                }
                double[][] new_w1_1 = ((double[][])(new double[][]{}));
                java.math.BigInteger i2_1 = java.math.BigInteger.valueOf(0);
                while (i2_1.compareTo(java.math.BigInteger.valueOf(3)) < 0) {
                    double[] w1row_1 = ((double[])(((double[])(net.w1[_idx((net.w1).length, ((java.math.BigInteger)(i2_1)).longValue())]))));
                    j_3 = java.math.BigInteger.valueOf(0);
                    while (j_3.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
w1row_1[(int)(((java.math.BigInteger)(j_3)).longValue())] = (double)((double)(w1row_1[_idx((w1row_1).length, ((java.math.BigInteger)(j_3)).longValue())]) + (double)((double)(inp_1[_idx((inp_1).length, ((java.math.BigInteger)(i2_1)).longValue())]) * (double)(delta_hidden1_1[_idx((delta_hidden1_1).length, ((java.math.BigInteger)(j_3)).longValue())])));
                        j_3 = j_3.add(java.math.BigInteger.valueOf(1));
                    }
                    new_w1_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_w1_1), java.util.stream.Stream.of(new double[][]{((double[])(w1row_1))})).toArray(double[][]::new)));
                    i2_1 = i2_1.add(java.math.BigInteger.valueOf(1));
                }
net.w1 = new_w1_1;
                s_1 = s_1.add(java.math.BigInteger.valueOf(1));
            }
            iter = iter.add(java.math.BigInteger.valueOf(1));
        }
    }

    static java.math.BigInteger predict(Network net, double[] input) {
        double out_2 = (double)(feedforward(net, ((double[])(input))));
        if ((double)(out_2) > (double)(0.6)) {
            return java.math.BigInteger.valueOf(1);
        }
        return java.math.BigInteger.valueOf(0);
    }

    static java.math.BigInteger example() {
        double[][] inputs = ((double[][])(new double[][]{((double[])(new double[]{(double)(0.0), (double)(0.0), (double)(0.0)})), ((double[])(new double[]{(double)(0.0), (double)(0.0), (double)(1.0)})), ((double[])(new double[]{(double)(0.0), (double)(1.0), (double)(0.0)})), ((double[])(new double[]{(double)(0.0), (double)(1.0), (double)(1.0)})), ((double[])(new double[]{(double)(1.0), (double)(0.0), (double)(0.0)})), ((double[])(new double[]{(double)(1.0), (double)(0.0), (double)(1.0)})), ((double[])(new double[]{(double)(1.0), (double)(1.0), (double)(0.0)})), ((double[])(new double[]{(double)(1.0), (double)(1.0), (double)(1.0)}))}));
        double[] outputs_1 = ((double[])(new double[]{(double)(0.0), (double)(1.0), (double)(1.0), (double)(0.0), (double)(1.0), (double)(0.0), (double)(0.0), (double)(1.0)}));
        Network net_1 = new_network();
        train(net_1, ((double[][])(inputs)), ((double[])(outputs_1)), java.math.BigInteger.valueOf(10));
        java.math.BigInteger result_1 = predict(net_1, ((double[])(new double[]{(double)(1.0), (double)(1.0), (double)(1.0)})));
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
