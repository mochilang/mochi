public class Main {
    static int seed = 0;
    static class Layer {
        int units;
        double[][] weight;
        double[] bias;
        double[] output;
        double[] xdata;
        double learn_rate;
        Layer(int units, double[][] weight, double[] bias, double[] output, double[] xdata, double learn_rate) {
            this.units = units;
            this.weight = weight;
            this.bias = bias;
            this.output = output;
            this.xdata = xdata;
            this.learn_rate = learn_rate;
        }
        Layer() {}
        @Override public String toString() {
            return String.format("{'units': %s, 'weight': %s, 'bias': %s, 'output': %s, 'xdata': %s, 'learn_rate': %s}", String.valueOf(units), String.valueOf(weight), String.valueOf(bias), String.valueOf(output), String.valueOf(xdata), String.valueOf(learn_rate));
        }
    }

    static class Data {
        double[][] x;
        double[][] y;
        Data(double[][] x, double[][] y) {
            this.x = x;
            this.y = y;
        }
        Data() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }


    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random() {
        return (1.0 * rand()) / 2147483648.0;
    }

    static double expApprox(double x) {
        double y = x;
        boolean is_neg = false;
        if (x < 0.0) {
            is_neg = true;
            y = -x;
        }
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 30) {
            term = term * y / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        if (((Boolean)(is_neg))) {
            return 1.0 / sum;
        }
        return sum;
    }

    static double sigmoid(double z) {
        return 1.0 / (1.0 + expApprox(-z));
    }

    static double[] sigmoid_vec(double[] v) {
        double[] res = ((double[])(new double[]{}));
        int i = 0;
        while (i < v.length) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(sigmoid(v[i]))).toArray()));
            i = i + 1;
        }
        return res;
    }

    static double[] sigmoid_derivative(double[] out) {
        double[] res_1 = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < out.length) {
            double val = out[i_1];
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(val * (1.0 - val))).toArray()));
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static double[] random_vector(int n) {
        double[] v = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < n) {
            v = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(v), java.util.stream.DoubleStream.of(random() - 0.5)).toArray()));
            i_2 = i_2 + 1;
        }
        return v;
    }

    static double[][] random_matrix(int r, int c) {
        double[][] m = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < r) {
            m = ((double[][])(appendObj(m, random_vector(c))));
            i_3 = i_3 + 1;
        }
        return m;
    }

    static double[] matvec(double[][] mat, double[] vec) {
        double[] res_2 = ((double[])(new double[]{}));
        int i_4 = 0;
        while (i_4 < mat.length) {
            double s = 0.0;
            int j = 0;
            while (j < vec.length) {
                s = s + mat[i_4][j] * vec[j];
                j = j + 1;
            }
            res_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_2), java.util.stream.DoubleStream.of(s)).toArray()));
            i_4 = i_4 + 1;
        }
        return res_2;
    }

    static double[] matTvec(double[][] mat, double[] vec) {
        int cols = mat[0].length;
        double[] res_3 = ((double[])(new double[]{}));
        int j_1 = 0;
        while (j_1 < cols) {
            double s_1 = 0.0;
            int i_5 = 0;
            while (i_5 < mat.length) {
                s_1 = s_1 + mat[i_5][j_1] * vec[i_5];
                i_5 = i_5 + 1;
            }
            res_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_3), java.util.stream.DoubleStream.of(s_1)).toArray()));
            j_1 = j_1 + 1;
        }
        return res_3;
    }

    static double[] vec_sub(double[] a, double[] b) {
        double[] res_4 = ((double[])(new double[]{}));
        int i_6 = 0;
        while (i_6 < a.length) {
            res_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_4), java.util.stream.DoubleStream.of(a[i_6] - b[i_6])).toArray()));
            i_6 = i_6 + 1;
        }
        return res_4;
    }

    static double[] vec_mul(double[] a, double[] b) {
        double[] res_5 = ((double[])(new double[]{}));
        int i_7 = 0;
        while (i_7 < a.length) {
            res_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_5), java.util.stream.DoubleStream.of(a[i_7] * b[i_7])).toArray()));
            i_7 = i_7 + 1;
        }
        return res_5;
    }

    static double[] vec_scalar_mul(double[] v, double s) {
        double[] res_6 = ((double[])(new double[]{}));
        int i_8 = 0;
        while (i_8 < v.length) {
            res_6 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_6), java.util.stream.DoubleStream.of(v[i_8] * s)).toArray()));
            i_8 = i_8 + 1;
        }
        return res_6;
    }

    static double[][] outer(double[] a, double[] b) {
        double[][] res_7 = ((double[][])(new double[][]{}));
        int i_9 = 0;
        while (i_9 < a.length) {
            double[] row = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < b.length) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(a[i_9] * b[j_2])).toArray()));
                j_2 = j_2 + 1;
            }
            res_7 = ((double[][])(appendObj(res_7, row)));
            i_9 = i_9 + 1;
        }
        return res_7;
    }

    static double[][] mat_scalar_mul(double[][] mat, double s) {
        double[][] res_8 = ((double[][])(new double[][]{}));
        int i_10 = 0;
        while (i_10 < mat.length) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_3 = 0;
            while (j_3 < mat[i_10].length) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(mat[i_10][j_3] * s)).toArray()));
                j_3 = j_3 + 1;
            }
            res_8 = ((double[][])(appendObj(res_8, row_1)));
            i_10 = i_10 + 1;
        }
        return res_8;
    }

    static double[][] mat_sub(double[][] a, double[][] b) {
        double[][] res_9 = ((double[][])(new double[][]{}));
        int i_11 = 0;
        while (i_11 < a.length) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_4 = 0;
            while (j_4 < a[i_11].length) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(a[i_11][j_4] - b[i_11][j_4])).toArray()));
                j_4 = j_4 + 1;
            }
            res_9 = ((double[][])(appendObj(res_9, row_2)));
            i_11 = i_11 + 1;
        }
        return res_9;
    }

    static Layer init_layer(int units, int back_units, double lr) {
        return new Layer(units, random_matrix(units, back_units), random_vector(units), new double[]{}, new double[]{}, lr);
    }

    static Layer[] forward(Layer[] layers, double[] x) {
        double[] data = ((double[])(x));
        int i_12 = 0;
        while (i_12 < layers.length) {
            Layer layer = layers[i_12];
layer.xdata = data;
            if (i_12 == 0) {
layer.output = data;
            } else {
                double[] z = ((double[])(vec_sub(((double[])(matvec(((double[][])(layer.weight)), ((double[])(data))))), ((double[])(layer.bias)))));
layer.output = sigmoid_vec(((double[])(z)));
                data = ((double[])(layer.output));
            }
layers[i_12] = layer;
            i_12 = i_12 + 1;
        }
        return layers;
    }

    static Layer[] backward(Layer[] layers, double[] grad) {
        double[] g = ((double[])(grad));
        int i_13 = layers.length - 1;
        while (i_13 > 0) {
            Layer layer_1 = layers[i_13];
            double[] deriv = ((double[])(sigmoid_derivative(((double[])(layer_1.output)))));
            double[] delta = ((double[])(vec_mul(((double[])(g)), ((double[])(deriv)))));
            double[][] grad_w = ((double[][])(outer(((double[])(delta)), ((double[])(layer_1.xdata)))));
layer_1.weight = mat_sub(((double[][])(layer_1.weight)), ((double[][])(mat_scalar_mul(((double[][])(grad_w)), layer_1.learn_rate))));
layer_1.bias = vec_sub(((double[])(layer_1.bias)), ((double[])(vec_scalar_mul(((double[])(delta)), layer_1.learn_rate))));
            g = ((double[])(matTvec(((double[][])(layer_1.weight)), ((double[])(delta)))));
layers[i_13] = layer_1;
            i_13 = i_13 - 1;
        }
        return layers;
    }

    static double calc_loss(double[] y, double[] yhat) {
        double s_2 = 0.0;
        int i_14 = 0;
        while (i_14 < y.length) {
            double d = y[i_14] - yhat[i_14];
            s_2 = s_2 + d * d;
            i_14 = i_14 + 1;
        }
        return s_2;
    }

    static double[] calc_gradient(double[] y, double[] yhat) {
        double[] g_1 = ((double[])(new double[]{}));
        int i_15 = 0;
        while (i_15 < y.length) {
            g_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(g_1), java.util.stream.DoubleStream.of(2.0 * (yhat[i_15] - y[i_15]))).toArray()));
            i_15 = i_15 + 1;
        }
        return g_1;
    }

    static double train(Layer[] layers, double[][] xdata, double[][] ydata, int rounds, double acc) {
        int r = 0;
        while (r < rounds) {
            int i_16 = 0;
            while (i_16 < xdata.length) {
                layers = ((Layer[])(forward(((Layer[])(layers)), ((double[])(xdata[i_16])))));
                double[] out = ((double[])(layers[layers.length - 1].output));
                double[] grad = ((double[])(calc_gradient(((double[])(ydata[i_16])), ((double[])(out)))));
                layers = ((Layer[])(backward(((Layer[])(layers)), ((double[])(grad)))));
                i_16 = i_16 + 1;
            }
            r = r + 1;
        }
        return 0.0;
    }

    static Data create_data() {
        double[][] x = ((double[][])(new double[][]{}));
        int i_17 = 0;
        while (i_17 < 10) {
            x = ((double[][])(appendObj(x, random_vector(10))));
            i_17 = i_17 + 1;
        }
        double[][] y_1 = ((double[][])(new double[][]{new double[]{0.8, 0.4}, new double[]{0.4, 0.3}, new double[]{0.34, 0.45}, new double[]{0.67, 0.32}, new double[]{0.88, 0.67}, new double[]{0.78, 0.77}, new double[]{0.55, 0.66}, new double[]{0.55, 0.43}, new double[]{0.54, 0.1}, new double[]{0.1, 0.5}}));
        return new Data(x, y_1);
    }

    static void main() {
        Data data_1 = create_data();
        double[][] x_1 = ((double[][])(data_1.x));
        double[][] y_2 = ((double[][])(data_1.y));
        Layer[] layers = ((Layer[])(new Layer[]{}));
        layers = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers), java.util.stream.Stream.of(init_layer(10, 0, 0.3))).toArray(Layer[]::new)));
        layers = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers), java.util.stream.Stream.of(init_layer(20, 10, 0.3))).toArray(Layer[]::new)));
        layers = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers), java.util.stream.Stream.of(init_layer(30, 20, 0.3))).toArray(Layer[]::new)));
        layers = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers), java.util.stream.Stream.of(init_layer(2, 30, 0.3))).toArray(Layer[]::new)));
        double final_mse = train(((Layer[])(layers)), ((double[][])(x_1)), ((double[][])(y_2)), 100, 0.01);
        System.out.println(final_mse);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
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
}
