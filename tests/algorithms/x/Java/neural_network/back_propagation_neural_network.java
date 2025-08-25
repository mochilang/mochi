public class Main {
    static java.math.BigInteger seed = java.math.BigInteger.valueOf(1);
    static class Layer {
        java.math.BigInteger units;
        double[][] weight;
        double[] bias;
        double[] output;
        double[] xdata;
        double learn_rate;
        Layer(java.math.BigInteger units, double[][] weight, double[] bias, double[] output, double[] xdata, double learn_rate) {
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


    static java.math.BigInteger rand() {
        seed = new java.math.BigInteger(String.valueOf((seed.multiply(java.math.BigInteger.valueOf(1103515245)).add(java.math.BigInteger.valueOf(12345))).remainder(java.math.BigInteger.valueOf(2147483648L))));
        return new java.math.BigInteger(String.valueOf(seed));
    }

    static double random() {
        return (double)((double)(((double)(1.0) * ((java.math.BigInteger)(rand())).doubleValue())) / (double)(2147483648.0));
    }

    static double expApprox(double x) {
        double y = (double)(x);
        boolean is_neg_1 = false;
        if ((double)(x) < (double)(0.0)) {
            is_neg_1 = true;
            y = (double)(-x);
        }
        double term_1 = (double)(1.0);
        double sum_1 = (double)(1.0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(1);
        while (n_1.compareTo(java.math.BigInteger.valueOf(30)) < 0) {
            term_1 = (double)((double)((double)(term_1) * (double)(y)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = new java.math.BigInteger(String.valueOf(n_1.add(java.math.BigInteger.valueOf(1))));
        }
        if (is_neg_1) {
            return (double)((double)(1.0) / (double)(sum_1));
        }
        return (double)(sum_1);
    }

    static double sigmoid(double z) {
        return (double)((double)(1.0) / (double)(((double)(1.0) + (double)(expApprox((double)(-z))))));
    }

    static double[] sigmoid_vec(double[] v) {
        double[] res = ((double[])(new double[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(v.length))) < 0) {
            res = ((double[])(appendDouble(res, (double)(sigmoid((double)(v[_idx((v).length, ((java.math.BigInteger)(i_1)).longValue())]))))));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res));
    }

    static double[] sigmoid_derivative(double[] out) {
        double[] res_1 = ((double[])(new double[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(out.length))) < 0) {
            double val_1 = (double)(out[_idx((out).length, ((java.math.BigInteger)(i_3)).longValue())]);
            res_1 = ((double[])(appendDouble(res_1, (double)((double)(val_1) * (double)(((double)(1.0) - (double)(val_1)))))));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_1));
    }

    static double[] random_vector(java.math.BigInteger n) {
        double[] v = ((double[])(new double[]{}));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(n) < 0) {
            v = ((double[])(appendDouble(v, (double)((double)(random()) - (double)(0.5)))));
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(v));
    }

    static double[][] random_matrix(java.math.BigInteger r, java.math.BigInteger c) {
        double[][] m = ((double[][])(new double[][]{}));
        java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(r) < 0) {
            m = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(m), java.util.stream.Stream.of(new double[][]{((double[])(random_vector(new java.math.BigInteger(String.valueOf(c)))))})).toArray(double[][]::new)));
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[][])(m));
    }

    static double[] matvec(double[][] mat, double[] vec) {
        double[] res_2 = ((double[])(new double[]{}));
        java.math.BigInteger i_9 = java.math.BigInteger.valueOf(0);
        while (i_9.compareTo(new java.math.BigInteger(String.valueOf(mat.length))) < 0) {
            double s_1 = (double)(0.0);
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(vec.length))) < 0) {
                s_1 = (double)((double)(s_1) + (double)((double)(mat[_idx((mat).length, ((java.math.BigInteger)(i_9)).longValue())][_idx((mat[_idx((mat).length, ((java.math.BigInteger)(i_9)).longValue())]).length, ((java.math.BigInteger)(j_1)).longValue())]) * (double)(vec[_idx((vec).length, ((java.math.BigInteger)(j_1)).longValue())])));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            res_2 = ((double[])(appendDouble(res_2, (double)(s_1))));
            i_9 = new java.math.BigInteger(String.valueOf(i_9.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_2));
    }

    static double[] matTvec(double[][] mat, double[] vec) {
        java.math.BigInteger cols = new java.math.BigInteger(String.valueOf(mat[_idx((mat).length, 0L)].length));
        double[] res_4 = ((double[])(new double[]{}));
        java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
        while (j_3.compareTo(cols) < 0) {
            double s_3 = (double)(0.0);
            java.math.BigInteger i_11 = java.math.BigInteger.valueOf(0);
            while (i_11.compareTo(new java.math.BigInteger(String.valueOf(mat.length))) < 0) {
                s_3 = (double)((double)(s_3) + (double)((double)(mat[_idx((mat).length, ((java.math.BigInteger)(i_11)).longValue())][_idx((mat[_idx((mat).length, ((java.math.BigInteger)(i_11)).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())]) * (double)(vec[_idx((vec).length, ((java.math.BigInteger)(i_11)).longValue())])));
                i_11 = new java.math.BigInteger(String.valueOf(i_11.add(java.math.BigInteger.valueOf(1))));
            }
            res_4 = ((double[])(appendDouble(res_4, (double)(s_3))));
            j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_4));
    }

    static double[] vec_sub(double[] a, double[] b) {
        double[] res_5 = ((double[])(new double[]{}));
        java.math.BigInteger i_13 = java.math.BigInteger.valueOf(0);
        while (i_13.compareTo(new java.math.BigInteger(String.valueOf(a.length))) < 0) {
            res_5 = ((double[])(appendDouble(res_5, (double)((double)(a[_idx((a).length, ((java.math.BigInteger)(i_13)).longValue())]) - (double)(b[_idx((b).length, ((java.math.BigInteger)(i_13)).longValue())])))));
            i_13 = new java.math.BigInteger(String.valueOf(i_13.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_5));
    }

    static double[] vec_mul(double[] a, double[] b) {
        double[] res_6 = ((double[])(new double[]{}));
        java.math.BigInteger i_15 = java.math.BigInteger.valueOf(0);
        while (i_15.compareTo(new java.math.BigInteger(String.valueOf(a.length))) < 0) {
            res_6 = ((double[])(appendDouble(res_6, (double)((double)(a[_idx((a).length, ((java.math.BigInteger)(i_15)).longValue())]) * (double)(b[_idx((b).length, ((java.math.BigInteger)(i_15)).longValue())])))));
            i_15 = new java.math.BigInteger(String.valueOf(i_15.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_6));
    }

    static double[] vec_scalar_mul(double[] v, double s) {
        double[] res_7 = ((double[])(new double[]{}));
        java.math.BigInteger i_17 = java.math.BigInteger.valueOf(0);
        while (i_17.compareTo(new java.math.BigInteger(String.valueOf(v.length))) < 0) {
            res_7 = ((double[])(appendDouble(res_7, (double)((double)(v[_idx((v).length, ((java.math.BigInteger)(i_17)).longValue())]) * (double)(s)))));
            i_17 = new java.math.BigInteger(String.valueOf(i_17.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_7));
    }

    static double[][] outer(double[] a, double[] b) {
        double[][] res_8 = ((double[][])(new double[][]{}));
        java.math.BigInteger i_19 = java.math.BigInteger.valueOf(0);
        while (i_19.compareTo(new java.math.BigInteger(String.valueOf(a.length))) < 0) {
            double[] row_1 = ((double[])(new double[]{}));
            java.math.BigInteger j_5 = java.math.BigInteger.valueOf(0);
            while (j_5.compareTo(new java.math.BigInteger(String.valueOf(b.length))) < 0) {
                row_1 = ((double[])(appendDouble(row_1, (double)((double)(a[_idx((a).length, ((java.math.BigInteger)(i_19)).longValue())]) * (double)(b[_idx((b).length, ((java.math.BigInteger)(j_5)).longValue())])))));
                j_5 = new java.math.BigInteger(String.valueOf(j_5.add(java.math.BigInteger.valueOf(1))));
            }
            res_8 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_8), java.util.stream.Stream.of(new double[][]{((double[])(row_1))})).toArray(double[][]::new)));
            i_19 = new java.math.BigInteger(String.valueOf(i_19.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[][])(res_8));
    }

    static double[][] mat_scalar_mul(double[][] mat, double s) {
        double[][] res_9 = ((double[][])(new double[][]{}));
        java.math.BigInteger i_21 = java.math.BigInteger.valueOf(0);
        while (i_21.compareTo(new java.math.BigInteger(String.valueOf(mat.length))) < 0) {
            double[] row_3 = ((double[])(new double[]{}));
            java.math.BigInteger j_7 = java.math.BigInteger.valueOf(0);
            while (j_7.compareTo(new java.math.BigInteger(String.valueOf(mat[_idx((mat).length, ((java.math.BigInteger)(i_21)).longValue())].length))) < 0) {
                row_3 = ((double[])(appendDouble(row_3, (double)((double)(mat[_idx((mat).length, ((java.math.BigInteger)(i_21)).longValue())][_idx((mat[_idx((mat).length, ((java.math.BigInteger)(i_21)).longValue())]).length, ((java.math.BigInteger)(j_7)).longValue())]) * (double)(s)))));
                j_7 = new java.math.BigInteger(String.valueOf(j_7.add(java.math.BigInteger.valueOf(1))));
            }
            res_9 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_9), java.util.stream.Stream.of(new double[][]{((double[])(row_3))})).toArray(double[][]::new)));
            i_21 = new java.math.BigInteger(String.valueOf(i_21.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[][])(res_9));
    }

    static double[][] mat_sub(double[][] a, double[][] b) {
        double[][] res_10 = ((double[][])(new double[][]{}));
        java.math.BigInteger i_23 = java.math.BigInteger.valueOf(0);
        while (i_23.compareTo(new java.math.BigInteger(String.valueOf(a.length))) < 0) {
            double[] row_5 = ((double[])(new double[]{}));
            java.math.BigInteger j_9 = java.math.BigInteger.valueOf(0);
            while (j_9.compareTo(new java.math.BigInteger(String.valueOf(a[_idx((a).length, ((java.math.BigInteger)(i_23)).longValue())].length))) < 0) {
                row_5 = ((double[])(appendDouble(row_5, (double)((double)(a[_idx((a).length, ((java.math.BigInteger)(i_23)).longValue())][_idx((a[_idx((a).length, ((java.math.BigInteger)(i_23)).longValue())]).length, ((java.math.BigInteger)(j_9)).longValue())]) - (double)(b[_idx((b).length, ((java.math.BigInteger)(i_23)).longValue())][_idx((b[_idx((b).length, ((java.math.BigInteger)(i_23)).longValue())]).length, ((java.math.BigInteger)(j_9)).longValue())])))));
                j_9 = new java.math.BigInteger(String.valueOf(j_9.add(java.math.BigInteger.valueOf(1))));
            }
            res_10 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_10), java.util.stream.Stream.of(new double[][]{((double[])(row_5))})).toArray(double[][]::new)));
            i_23 = new java.math.BigInteger(String.valueOf(i_23.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[][])(res_10));
    }

    static Layer init_layer(java.math.BigInteger units, java.math.BigInteger back_units, double lr) {
        return new Layer(new java.math.BigInteger(String.valueOf(units)), ((double[][])(random_matrix(new java.math.BigInteger(String.valueOf(units)), new java.math.BigInteger(String.valueOf(back_units))))), ((double[])(random_vector(new java.math.BigInteger(String.valueOf(units))))), ((double[])(new double[]{})), ((double[])(new double[]{})), (double)(lr));
    }

    static Layer[] forward(Layer[] layers, double[] x) {
        double[] data = ((double[])(x));
        java.math.BigInteger i_25 = java.math.BigInteger.valueOf(0);
        while (i_25.compareTo(new java.math.BigInteger(String.valueOf(layers.length))) < 0) {
            Layer layer_1 = layers[_idx((layers).length, ((java.math.BigInteger)(i_25)).longValue())];
layer_1.xdata = data;
            if (i_25.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
layer_1.output = data;
            } else {
                double[] z_1 = ((double[])(vec_sub(((double[])(matvec(((double[][])(layer_1.weight)), ((double[])(data))))), ((double[])(layer_1.bias)))));
layer_1.output = sigmoid_vec(((double[])(z_1)));
                data = ((double[])(layer_1.output));
            }
layers[(int)(((java.math.BigInteger)(i_25)).longValue())] = layer_1;
            i_25 = new java.math.BigInteger(String.valueOf(i_25.add(java.math.BigInteger.valueOf(1))));
        }
        return ((Layer[])(layers));
    }

    static Layer[] backward(Layer[] layers, double[] grad) {
        double[] g = ((double[])(grad));
        java.math.BigInteger i_27 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(layers.length)).subtract(java.math.BigInteger.valueOf(1))));
        while (i_27.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            Layer layer_3 = layers[_idx((layers).length, ((java.math.BigInteger)(i_27)).longValue())];
            double[] deriv_1 = ((double[])(sigmoid_derivative(((double[])(layer_3.output)))));
            double[] delta_1 = ((double[])(vec_mul(((double[])(g)), ((double[])(deriv_1)))));
            double[][] grad_w_1 = ((double[][])(outer(((double[])(delta_1)), ((double[])(layer_3.xdata)))));
layer_3.weight = mat_sub(((double[][])(layer_3.weight)), ((double[][])(mat_scalar_mul(((double[][])(grad_w_1)), (double)(layer_3.learn_rate)))));
layer_3.bias = vec_sub(((double[])(layer_3.bias)), ((double[])(vec_scalar_mul(((double[])(delta_1)), (double)(layer_3.learn_rate)))));
            g = ((double[])(matTvec(((double[][])(layer_3.weight)), ((double[])(delta_1)))));
layers[(int)(((java.math.BigInteger)(i_27)).longValue())] = layer_3;
            i_27 = new java.math.BigInteger(String.valueOf(i_27.subtract(java.math.BigInteger.valueOf(1))));
        }
        return ((Layer[])(layers));
    }

    static double calc_loss(double[] y, double[] yhat) {
        double s_4 = (double)(0.0);
        java.math.BigInteger i_29 = java.math.BigInteger.valueOf(0);
        while (i_29.compareTo(new java.math.BigInteger(String.valueOf(y.length))) < 0) {
            double d_1 = (double)((double)(y[_idx((y).length, ((java.math.BigInteger)(i_29)).longValue())]) - (double)(yhat[_idx((yhat).length, ((java.math.BigInteger)(i_29)).longValue())]));
            s_4 = (double)((double)(s_4) + (double)((double)(d_1) * (double)(d_1)));
            i_29 = new java.math.BigInteger(String.valueOf(i_29.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(s_4);
    }

    static double[] calc_gradient(double[] y, double[] yhat) {
        double[] g_1 = ((double[])(new double[]{}));
        java.math.BigInteger i_31 = java.math.BigInteger.valueOf(0);
        while (i_31.compareTo(new java.math.BigInteger(String.valueOf(y.length))) < 0) {
            g_1 = ((double[])(appendDouble(g_1, (double)((double)(2.0) * (double)(((double)(yhat[_idx((yhat).length, ((java.math.BigInteger)(i_31)).longValue())]) - (double)(y[_idx((y).length, ((java.math.BigInteger)(i_31)).longValue())])))))));
            i_31 = new java.math.BigInteger(String.valueOf(i_31.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(g_1));
    }

    static double train(Layer[] layers, double[][] xdata, double[][] ydata, java.math.BigInteger rounds, double acc) {
        java.math.BigInteger r = java.math.BigInteger.valueOf(0);
        while (r.compareTo(rounds) < 0) {
            java.math.BigInteger i_33 = java.math.BigInteger.valueOf(0);
            while (i_33.compareTo(new java.math.BigInteger(String.valueOf(xdata.length))) < 0) {
                layers = ((Layer[])(forward(((Layer[])(layers)), ((double[])(xdata[_idx((xdata).length, ((java.math.BigInteger)(i_33)).longValue())])))));
                double[] out_1 = ((double[])(layers[_idx((layers).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(layers.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())].output));
                double[] grad_1 = ((double[])(calc_gradient(((double[])(ydata[_idx((ydata).length, ((java.math.BigInteger)(i_33)).longValue())])), ((double[])(out_1)))));
                layers = ((Layer[])(backward(((Layer[])(layers)), ((double[])(grad_1)))));
                i_33 = new java.math.BigInteger(String.valueOf(i_33.add(java.math.BigInteger.valueOf(1))));
            }
            r = new java.math.BigInteger(String.valueOf(r.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(0.0);
    }

    static Data create_data() {
        double[][] x = ((double[][])(new double[][]{}));
        java.math.BigInteger i_35 = java.math.BigInteger.valueOf(0);
        while (i_35.compareTo(java.math.BigInteger.valueOf(10)) < 0) {
            x = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(x), java.util.stream.Stream.of(new double[][]{((double[])(random_vector(java.math.BigInteger.valueOf(10))))})).toArray(double[][]::new)));
            i_35 = new java.math.BigInteger(String.valueOf(i_35.add(java.math.BigInteger.valueOf(1))));
        }
        double[][] y_2 = ((double[][])(new double[][]{((double[])(new double[]{(double)(0.8), (double)(0.4)})), ((double[])(new double[]{(double)(0.4), (double)(0.3)})), ((double[])(new double[]{(double)(0.34), (double)(0.45)})), ((double[])(new double[]{(double)(0.67), (double)(0.32)})), ((double[])(new double[]{(double)(0.88), (double)(0.67)})), ((double[])(new double[]{(double)(0.78), (double)(0.77)})), ((double[])(new double[]{(double)(0.55), (double)(0.66)})), ((double[])(new double[]{(double)(0.55), (double)(0.43)})), ((double[])(new double[]{(double)(0.54), (double)(0.1)})), ((double[])(new double[]{(double)(0.1), (double)(0.5)}))}));
        return new Data(((double[][])(x)), ((double[][])(y_2)));
    }

    static void main() {
        Data data_1 = create_data();
        double[][] x_2 = ((double[][])(data_1.x));
        double[][] y_4 = ((double[][])(data_1.y));
        Layer[] layers_1 = ((Layer[])(new Layer[]{}));
        layers_1 = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers_1), java.util.stream.Stream.of(init_layer(java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(0), (double)(0.3)))).toArray(Layer[]::new)));
        layers_1 = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers_1), java.util.stream.Stream.of(init_layer(java.math.BigInteger.valueOf(20), java.math.BigInteger.valueOf(10), (double)(0.3)))).toArray(Layer[]::new)));
        layers_1 = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers_1), java.util.stream.Stream.of(init_layer(java.math.BigInteger.valueOf(30), java.math.BigInteger.valueOf(20), (double)(0.3)))).toArray(Layer[]::new)));
        layers_1 = ((Layer[])(java.util.stream.Stream.concat(java.util.Arrays.stream(layers_1), java.util.stream.Stream.of(init_layer(java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(30), (double)(0.3)))).toArray(Layer[]::new)));
        double final_mse_1 = (double)(train(((Layer[])(layers_1)), ((double[][])(x_2)), ((double[][])(y_4)), java.math.BigInteger.valueOf(100), (double)(0.01)));
        System.out.println(final_mse_1);
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
