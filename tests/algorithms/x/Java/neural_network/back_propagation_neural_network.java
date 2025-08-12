public class Main {
    static long seed = 0;
    static class Layer {
        long units;
        double[][] weight;
        double[] bias;
        double[] output;
        double[] xdata;
        double learn_rate;
        Layer(long units, double[][] weight, double[] bias, double[] output, double[] xdata, double learn_rate) {
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


    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * (long)(1103515245)) + (long)(12345)))), 2147483648L))));
        return seed;
    }

    static double random() {
        return (1.0 * (double)(rand())) / 2147483648.0;
    }

    static double expApprox(double x) {
        double y = (double)(x);
        boolean is_neg_1 = false;
        if ((double)(x) < 0.0) {
            is_neg_1 = true;
            y = (double)(-x);
        }
        double term_1 = 1.0;
        double sum_1 = 1.0;
        long n_1 = 1L;
        while (n_1 < (long)(30)) {
            term_1 = (double)(term_1) * (double)(y) / (((Number)(n_1)).doubleValue());
            sum_1 = (double)(sum_1) + (double)(term_1);
            n_1 = (long)(n_1 + (long)(1));
        }
        if (is_neg_1) {
            return 1.0 / (double)(sum_1);
        }
        return sum_1;
    }

    static double sigmoid(double z) {
        return 1.0 / (1.0 + (double)(expApprox((double)(-z))));
    }

    static double[] sigmoid_vec(double[] v) {
        double[] res = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(v.length)) {
            res = ((double[])(appendDouble(res, (double)(sigmoid((double)(_getd(v, (int)((long)(i_1)))))))));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return res;
    }

    static double[] sigmoid_derivative(double[] out) {
        double[] res_1 = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(out.length)) {
            double val_1 = (double)(_getd(out, (int)((long)(i_3))));
            res_1 = ((double[])(appendDouble(res_1, (double)(val_1) * (1.0 - (double)(val_1)))));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return res_1;
    }

    static double[] random_vector(long n) {
        double[] v = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < n) {
            v = ((double[])(appendDouble(v, (double)(random()) - 0.5)));
            i_5 = (long)((long)(i_5) + (long)(1));
        }
        return v;
    }

    static double[][] random_matrix(long r, long c) {
        double[][] m = ((double[][])(new double[][]{}));
        long i_7 = 0L;
        while ((long)(i_7) < r) {
            m = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(m), java.util.stream.Stream.of(random_vector(c))).toArray(double[][]::new)));
            i_7 = (long)((long)(i_7) + (long)(1));
        }
        return m;
    }

    static double[] matvec(double[][] mat, double[] vec) {
        double[] res_2 = ((double[])(new double[]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(mat.length)) {
            double s_1 = 0.0;
            long j_1 = 0L;
            while ((long)(j_1) < (long)(vec.length)) {
                s_1 = s_1 + (double)(_getd(((double[])_geto(mat, (int)((long)(i_9)))), (int)((long)(j_1)))) * (double)(_getd(vec, (int)((long)(j_1))));
                j_1 = (long)((long)(j_1) + (long)(1));
            }
            res_2 = ((double[])(appendDouble(res_2, s_1)));
            i_9 = (long)((long)(i_9) + (long)(1));
        }
        return res_2;
    }

    static double[] matTvec(double[][] mat, double[] vec) {
        long cols = (long)(((double[])_geto(mat, (int)((long)(0)))).length);
        double[] res_4 = ((double[])(new double[]{}));
        long j_3 = 0L;
        while ((long)(j_3) < (long)(cols)) {
            double s_3 = 0.0;
            long i_11 = 0L;
            while ((long)(i_11) < (long)(mat.length)) {
                s_3 = s_3 + (double)(_getd(((double[])_geto(mat, (int)((long)(i_11)))), (int)((long)(j_3)))) * (double)(_getd(vec, (int)((long)(i_11))));
                i_11 = (long)((long)(i_11) + (long)(1));
            }
            res_4 = ((double[])(appendDouble(res_4, s_3)));
            j_3 = (long)((long)(j_3) + (long)(1));
        }
        return res_4;
    }

    static double[] vec_sub(double[] a, double[] b) {
        double[] res_5 = ((double[])(new double[]{}));
        long i_13 = 0L;
        while ((long)(i_13) < (long)(a.length)) {
            res_5 = ((double[])(appendDouble(res_5, (double)(_getd(a, (int)((long)(i_13)))) - (double)(_getd(b, (int)((long)(i_13)))))));
            i_13 = (long)((long)(i_13) + (long)(1));
        }
        return res_5;
    }

    static double[] vec_mul(double[] a, double[] b) {
        double[] res_6 = ((double[])(new double[]{}));
        long i_15 = 0L;
        while ((long)(i_15) < (long)(a.length)) {
            res_6 = ((double[])(appendDouble(res_6, (double)(_getd(a, (int)((long)(i_15)))) * (double)(_getd(b, (int)((long)(i_15)))))));
            i_15 = (long)((long)(i_15) + (long)(1));
        }
        return res_6;
    }

    static double[] vec_scalar_mul(double[] v, double s) {
        double[] res_7 = ((double[])(new double[]{}));
        long i_17 = 0L;
        while ((long)(i_17) < (long)(v.length)) {
            res_7 = ((double[])(appendDouble(res_7, (double)(_getd(v, (int)((long)(i_17)))) * (double)(s))));
            i_17 = (long)((long)(i_17) + (long)(1));
        }
        return res_7;
    }

    static double[][] outer(double[] a, double[] b) {
        double[][] res_8 = ((double[][])(new double[][]{}));
        long i_19 = 0L;
        while ((long)(i_19) < (long)(a.length)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(b.length)) {
                row_1 = ((double[])(appendDouble(row_1, (double)(_getd(a, (int)((long)(i_19)))) * (double)(_getd(b, (int)((long)(j_5)))))));
                j_5 = (long)((long)(j_5) + (long)(1));
            }
            res_8 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_8), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_19 = (long)((long)(i_19) + (long)(1));
        }
        return res_8;
    }

    static double[][] mat_scalar_mul(double[][] mat, double s) {
        double[][] res_9 = ((double[][])(new double[][]{}));
        long i_21 = 0L;
        while ((long)(i_21) < (long)(mat.length)) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_7 = 0L;
            while ((long)(j_7) < (long)(((double[])_geto(mat, (int)((long)(i_21)))).length)) {
                row_3 = ((double[])(appendDouble(row_3, (double)(_getd(((double[])_geto(mat, (int)((long)(i_21)))), (int)((long)(j_7)))) * (double)(s))));
                j_7 = (long)((long)(j_7) + (long)(1));
            }
            res_9 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_9), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            i_21 = (long)((long)(i_21) + (long)(1));
        }
        return res_9;
    }

    static double[][] mat_sub(double[][] a, double[][] b) {
        double[][] res_10 = ((double[][])(new double[][]{}));
        long i_23 = 0L;
        while ((long)(i_23) < (long)(a.length)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_9 = 0L;
            while ((long)(j_9) < (long)(((double[])_geto(a, (int)((long)(i_23)))).length)) {
                row_5 = ((double[])(appendDouble(row_5, (double)(_getd(((double[])_geto(a, (int)((long)(i_23)))), (int)((long)(j_9)))) - (double)(_getd(((double[])_geto(b, (int)((long)(i_23)))), (int)((long)(j_9)))))));
                j_9 = (long)((long)(j_9) + (long)(1));
            }
            res_10 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_10), java.util.stream.Stream.of(row_5)).toArray(double[][]::new)));
            i_23 = (long)((long)(i_23) + (long)(1));
        }
        return res_10;
    }

    static Layer init_layer(long units, long back_units, double lr) {
        return new Layer(units, random_matrix(units, back_units), random_vector(units), new double[]{}, new double[]{}, lr);
    }

    static Layer[] forward(Layer[] layers, double[] x) {
        double[] data = ((double[])(x));
        long i_25 = 0L;
        while ((long)(i_25) < (long)(layers.length)) {
            Layer layer_1 = ((Layer)_geto(layers, (int)((long)(i_25))));
layer_1.xdata = data;
            if ((long)(i_25) == (long)(0)) {
layer_1.output = data;
            } else {
                double[] z_1 = ((double[])(vec_sub(((double[])(matvec(((double[][])(layer_1.weight)), ((double[])(data))))), ((double[])(layer_1.bias)))));
layer_1.output = sigmoid_vec(((double[])(z_1)));
                data = ((double[])(layer_1.output));
            }
layers[(int)((long)(i_25))] = layer_1;
            i_25 = (long)((long)(i_25) + (long)(1));
        }
        return layers;
    }

    static Layer[] backward(Layer[] layers, double[] grad) {
        double[] g = ((double[])(grad));
        long i_27 = (long)((long)(layers.length) - (long)(1));
        while ((long)(i_27) > (long)(0)) {
            Layer layer_3 = ((Layer)_geto(layers, (int)((long)(i_27))));
            double[] deriv_1 = ((double[])(sigmoid_derivative(((double[])(layer_3.output)))));
            double[] delta_1 = ((double[])(vec_mul(((double[])(g)), ((double[])(deriv_1)))));
            double[][] grad_w_1 = ((double[][])(outer(((double[])(delta_1)), ((double[])(layer_3.xdata)))));
layer_3.weight = mat_sub(((double[][])(layer_3.weight)), ((double[][])(mat_scalar_mul(((double[][])(grad_w_1)), layer_3.learn_rate))));
layer_3.bias = vec_sub(((double[])(layer_3.bias)), ((double[])(vec_scalar_mul(((double[])(delta_1)), layer_3.learn_rate))));
            g = ((double[])(matTvec(((double[][])(layer_3.weight)), ((double[])(delta_1)))));
layers[(int)((long)(i_27))] = layer_3;
            i_27 = (long)((long)(i_27) - (long)(1));
        }
        return layers;
    }

    static double calc_loss(double[] y, double[] yhat) {
        double s_4 = 0.0;
        long i_29 = 0L;
        while ((long)(i_29) < (long)(y.length)) {
            double d_1 = (double)(_getd(y, (int)((long)(i_29)))) - (double)(_getd(yhat, (int)((long)(i_29))));
            s_4 = s_4 + d_1 * d_1;
            i_29 = (long)((long)(i_29) + (long)(1));
        }
        return s_4;
    }

    static double[] calc_gradient(double[] y, double[] yhat) {
        double[] g_1 = ((double[])(new double[]{}));
        long i_31 = 0L;
        while ((long)(i_31) < (long)(y.length)) {
            g_1 = ((double[])(appendDouble(g_1, 2.0 * ((double)(_getd(yhat, (int)((long)(i_31)))) - (double)(_getd(y, (int)((long)(i_31))))))));
            i_31 = (long)((long)(i_31) + (long)(1));
        }
        return g_1;
    }

    static double train(Layer[] layers, double[][] xdata, double[][] ydata, long rounds, double acc) {
        long r = 0L;
        while ((long)(r) < rounds) {
            long i_33 = 0L;
            while ((long)(i_33) < (long)(xdata.length)) {
                layers = ((Layer[])(forward(((Layer[])(layers)), ((double[])(((double[])_geto(xdata, (int)((long)(i_33)))))))));
                double[] out_1 = ((double[])(((Layer)_geto(layers, (int)((long)((long)(layers.length) - (long)(1))))).output));
                double[] grad_1 = ((double[])(calc_gradient(((double[])(((double[])_geto(ydata, (int)((long)(i_33)))))), ((double[])(out_1)))));
                layers = ((Layer[])(backward(((Layer[])(layers)), ((double[])(grad_1)))));
                i_33 = (long)((long)(i_33) + (long)(1));
            }
            r = (long)((long)(r) + (long)(1));
        }
        return 0.0;
    }

    static Data create_data() {
        double[][] x = ((double[][])(new double[][]{}));
        long i_35 = 0L;
        while ((long)(i_35) < (long)(10)) {
            x = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(x), java.util.stream.Stream.of(random_vector(10L))).toArray(double[][]::new)));
            i_35 = (long)((long)(i_35) + (long)(1));
        }
        double[][] y_2 = ((double[][])(new double[][]{new double[]{0.8, 0.4}, new double[]{0.4, 0.3}, new double[]{0.34, 0.45}, new double[]{0.67, 0.32}, new double[]{0.88, 0.67}, new double[]{0.78, 0.77}, new double[]{0.55, 0.66}, new double[]{0.55, 0.43}, new double[]{0.54, 0.1}, new double[]{0.1, 0.5}}));
        return new Data(x, y_2);
    }

    static void main() {
        Data data_1 = create_data();
        double[][] x_2 = ((double[][])(data_1.x));
        double[][] y_4 = ((double[][])(data_1.y));
        Layer[] layers_1 = ((Layer[])(new Layer[]{}));
        layers_1 = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers_1), java.util.stream.Stream.of(init_layer(10L, 0L, 0.3))).toArray(Layer[]::new)));
        layers_1 = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers_1), java.util.stream.Stream.of(init_layer(20L, 10L, 0.3))).toArray(Layer[]::new)));
        layers_1 = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers_1), java.util.stream.Stream.of(init_layer(30L, 20L, 0.3))).toArray(Layer[]::new)));
        layers_1 = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers_1), java.util.stream.Stream.of(init_layer(2L, 30L, 0.3))).toArray(Layer[]::new)));
        double final_mse_1 = (double)(train(((Layer[])(layers_1)), ((double[][])(x_2)), ((double[][])(y_4)), 100L, 0.01));
        System.out.println(final_mse_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = (long)(1);
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

    static double _getd(double[] a, int i) {
        if (a == null) return 0.0;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0.0;
        return a[i];
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
