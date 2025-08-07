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
        double term = 1.0;
        int i = 1;
        while (i < 10) {
            term = term * x / ((Number)(i)).doubleValue();
            sum = sum + term;
            i = i + 1;
        }
        return sum;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + exp_approx(-x));
    }

    static double sigmoid_derivative(double x) {
        return x * (1.0 - x);
    }

    static Network new_network() {
        return new Network(new double[][]{new double[]{0.1, 0.2, 0.3, 0.4}, new double[]{0.5, 0.6, 0.7, 0.8}, new double[]{0.9, 1.0, 1.1, 1.2}}, new double[][]{new double[]{0.1, 0.2, 0.3}, new double[]{0.4, 0.5, 0.6}, new double[]{0.7, 0.8, 0.9}, new double[]{1.0, 1.1, 1.2}}, new double[][]{new double[]{0.1}, new double[]{0.2}, new double[]{0.3}});
    }

    static double feedforward(Network net, double[] input) {
        double[] hidden1 = ((double[])(new double[]{}));
        int j = 0;
        while (j < 4) {
            double sum1 = 0.0;
            int i_1 = 0;
            while (i_1 < 3) {
                sum1 = sum1 + input[i_1] * net.w1[i_1][j];
                i_1 = i_1 + 1;
            }
            hidden1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(hidden1), java.util.stream.DoubleStream.of(sigmoid(sum1))).toArray()));
            j = j + 1;
        }
        double[] hidden2 = ((double[])(new double[]{}));
        int k = 0;
        while (k < 3) {
            double sum2 = 0.0;
            int j2 = 0;
            while (j2 < 4) {
                sum2 = sum2 + hidden1[j2] * net.w2[j2][k];
                j2 = j2 + 1;
            }
            hidden2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(hidden2), java.util.stream.DoubleStream.of(sigmoid(sum2))).toArray()));
            k = k + 1;
        }
        double sum3 = 0.0;
        int k2 = 0;
        while (k2 < 3) {
            sum3 = sum3 + hidden2[k2] * net.w3[k2][0];
            k2 = k2 + 1;
        }
        double out = sigmoid(sum3);
        return out;
    }

    static void train(Network net, double[][] inputs, double[] outputs, int iterations) {
        int iter = 0;
        while (iter < iterations) {
            int s = 0;
            while (s < inputs.length) {
                double[] inp = ((double[])(inputs[s]));
                double target = outputs[s];
                double[] hidden1_1 = ((double[])(new double[]{}));
                int j_1 = 0;
                while (j_1 < 4) {
                    double sum1_1 = 0.0;
                    int i_2 = 0;
                    while (i_2 < 3) {
                        sum1_1 = sum1_1 + inp[i_2] * net.w1[i_2][j_1];
                        i_2 = i_2 + 1;
                    }
                    hidden1_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(hidden1_1), java.util.stream.DoubleStream.of(sigmoid(sum1_1))).toArray()));
                    j_1 = j_1 + 1;
                }
                double[] hidden2_1 = ((double[])(new double[]{}));
                int k_1 = 0;
                while (k_1 < 3) {
                    double sum2_1 = 0.0;
                    int j2_1 = 0;
                    while (j2_1 < 4) {
                        sum2_1 = sum2_1 + hidden1_1[j2_1] * net.w2[j2_1][k_1];
                        j2_1 = j2_1 + 1;
                    }
                    hidden2_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(hidden2_1), java.util.stream.DoubleStream.of(sigmoid(sum2_1))).toArray()));
                    k_1 = k_1 + 1;
                }
                double sum3_1 = 0.0;
                int k3 = 0;
                while (k3 < 3) {
                    sum3_1 = sum3_1 + hidden2_1[k3] * net.w3[k3][0];
                    k3 = k3 + 1;
                }
                double output = sigmoid(sum3_1);
                double error = target - output;
                double delta_output = error * sigmoid_derivative(output);
                double[][] new_w3 = ((double[][])(new double[][]{}));
                int k4 = 0;
                while (k4 < 3) {
                    double[] w3row = ((double[])(((double[])(net.w3[k4]))));
w3row[0] = w3row[0] + hidden2_1[k4] * delta_output;
                    new_w3 = ((double[][])(appendObj(new_w3, w3row)));
                    k4 = k4 + 1;
                }
net.w3 = new_w3;
                double[] delta_hidden2 = ((double[])(new double[]{}));
                int k5 = 0;
                while (k5 < 3) {
                    double[] row = ((double[])(net.w3[k5]));
                    double dh2 = row[0] * delta_output * sigmoid_derivative(hidden2_1[k5]);
                    delta_hidden2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(delta_hidden2), java.util.stream.DoubleStream.of(dh2)).toArray()));
                    k5 = k5 + 1;
                }
                double[][] new_w2 = ((double[][])(new double[][]{}));
                j_1 = 0;
                while (j_1 < 4) {
                    double[] w2row = ((double[])(((double[])(net.w2[j_1]))));
                    int k6 = 0;
                    while (k6 < 3) {
w2row[k6] = w2row[k6] + hidden1_1[j_1] * delta_hidden2[k6];
                        k6 = k6 + 1;
                    }
                    new_w2 = ((double[][])(appendObj(new_w2, w2row)));
                    j_1 = j_1 + 1;
                }
net.w2 = new_w2;
                double[] delta_hidden1 = ((double[])(new double[]{}));
                j_1 = 0;
                while (j_1 < 4) {
                    double sumdh = 0.0;
                    int k7 = 0;
                    while (k7 < 3) {
                        double[] row2 = ((double[])(net.w2[j_1]));
                        sumdh = sumdh + row2[k7] * delta_hidden2[k7];
                        k7 = k7 + 1;
                    }
                    delta_hidden1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(delta_hidden1), java.util.stream.DoubleStream.of(sumdh * sigmoid_derivative(hidden1_1[j_1]))).toArray()));
                    j_1 = j_1 + 1;
                }
                double[][] new_w1 = ((double[][])(new double[][]{}));
                int i2 = 0;
                while (i2 < 3) {
                    double[] w1row = ((double[])(((double[])(net.w1[i2]))));
                    j_1 = 0;
                    while (j_1 < 4) {
w1row[j_1] = w1row[j_1] + inp[i2] * delta_hidden1[j_1];
                        j_1 = j_1 + 1;
                    }
                    new_w1 = ((double[][])(appendObj(new_w1, w1row)));
                    i2 = i2 + 1;
                }
net.w1 = new_w1;
                s = s + 1;
            }
            iter = iter + 1;
        }
    }

    static int predict(Network net, double[] input) {
        double out_1 = feedforward(net, ((double[])(input)));
        if (out_1 > 0.6) {
            return 1;
        }
        return 0;
    }

    static int example() {
        double[][] inputs = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0}, new double[]{0.0, 1.0, 0.0}, new double[]{0.0, 1.0, 1.0}, new double[]{1.0, 0.0, 0.0}, new double[]{1.0, 0.0, 1.0}, new double[]{1.0, 1.0, 0.0}, new double[]{1.0, 1.0, 1.0}}));
        double[] outputs = ((double[])(new double[]{0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0}));
        Network net = new_network();
        train(net, ((double[][])(inputs)), ((double[])(outputs)), 10);
        int result = predict(net, ((double[])(new double[]{1.0, 1.0, 1.0})));
        System.out.println(_p(result));
        return result;
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
