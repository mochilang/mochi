public class Main {
    static class CNN {
        double[][][] conv_kernels;
        double[] conv_bias;
        long conv_step;
        long pool_size;
        double[][] w_hidden;
        double[][] w_out;
        double[] b_hidden;
        double[] b_out;
        double rate_weight;
        double rate_bias;
        CNN(double[][][] conv_kernels, double[] conv_bias, long conv_step, long pool_size, double[][] w_hidden, double[][] w_out, double[] b_hidden, double[] b_out, double rate_weight, double rate_bias) {
            this.conv_kernels = conv_kernels;
            this.conv_bias = conv_bias;
            this.conv_step = conv_step;
            this.pool_size = pool_size;
            this.w_hidden = w_hidden;
            this.w_out = w_out;
            this.b_hidden = b_hidden;
            this.b_out = b_out;
            this.rate_weight = rate_weight;
            this.rate_bias = rate_bias;
        }
        CNN() {}
        @Override public String toString() {
            return String.format("{'conv_kernels': %s, 'conv_bias': %s, 'conv_step': %s, 'pool_size': %s, 'w_hidden': %s, 'w_out': %s, 'b_hidden': %s, 'b_out': %s, 'rate_weight': %s, 'rate_bias': %s}", String.valueOf(conv_kernels), String.valueOf(conv_bias), String.valueOf(conv_step), String.valueOf(pool_size), String.valueOf(w_hidden), String.valueOf(w_out), String.valueOf(b_hidden), String.valueOf(b_out), String.valueOf(rate_weight), String.valueOf(rate_bias));
        }
    }

    static long seed = 0;
    static class TrainSample {
        double[][] image;
        double[] target;
        TrainSample(double[][] image, double[] target) {
            this.image = image;
            this.target = target;
        }
        TrainSample() {}
        @Override public String toString() {
            return String.format("{'image': %s, 'target': %s}", String.valueOf(image), String.valueOf(target));
        }
    }


    static double random() {
        seed = Math.floorMod(((long)((long)(seed) * (long)(13)) + (long)(7)), 100);
        return (((Number)(seed)).doubleValue()) / 100.0;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + (double)(exp((double)(-x))));
    }

    static double to_float(long x) {
        return (double)(x) * 1.0;
    }

    static double exp(double x) {
        double term = 1.0;
        double sum_1 = 1.0;
        long n_1 = 1L;
        while ((long)(n_1) < (long)(20)) {
            term = term * (double)(x) / (double)(to_float((long)(n_1)));
            sum_1 = sum_1 + term;
            n_1 = (long)((long)(n_1) + (long)(1));
        }
        return sum_1;
    }

    static double[][] convolve(double[][] data, double[][] kernel, long step, double bias) {
        long size_data = (long)(data.length);
        long size_kernel_1 = (long)(kernel.length);
        double[][] out_1 = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while (i_1 <= (long)((long)(size_data) - (long)(size_kernel_1))) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while (j_1 <= (long)((long)(size_data) - (long)(size_kernel_1))) {
                double sum_3 = 0.0;
                long a_1 = 0L;
                while (a_1 < (long)(size_kernel_1)) {
                    long b_1 = 0L;
                    while (b_1 < (long)(size_kernel_1)) {
                        sum_3 = (double)(sum_3) + (double)(_getd(((double[])_geto(data, (int)((long)(i_1 + a_1)))), (int)((long)(j_1 + b_1)))) * (double)(_getd(((double[])_geto(kernel, (int)((long)(a_1)))), (int)((long)(b_1))));
                        b_1 = (long)(b_1 + (long)(1));
                    }
                    a_1 = (long)(a_1 + (long)(1));
                }
                row_1 = ((double[])(appendDouble(row_1, (double)(sigmoid((double)(sum_3) - (double)(bias))))));
                j_1 = (long)(j_1 + step);
            }
            out_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(out_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_1 = (long)(i_1 + step);
        }
        return out_1;
    }

    static double[][] average_pool(double[][] map, long size) {
        double[][] out_2 = ((double[][])(new double[][]{}));
        long i_3 = 0L;
        while (i_3 < (long)(map.length)) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while (j_3 < (long)(((double[])_geto(map, (int)((long)(i_3)))).length)) {
                double sum_5 = 0.0;
                long a_3 = 0L;
                while (a_3 < size) {
                    long b_3 = 0L;
                    while (b_3 < size) {
                        sum_5 = (double)(sum_5) + (double)(_getd(((double[])_geto(map, (int)((long)(i_3 + a_3)))), (int)((long)(j_3 + b_3))));
                        b_3 = (long)(b_3 + (long)(1));
                    }
                    a_3 = (long)(a_3 + (long)(1));
                }
                row_3 = ((double[])(appendDouble(row_3, (double)(sum_5) / (((Number)((size * size))).doubleValue()))));
                j_3 = (long)(j_3 + size);
            }
            out_2 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(out_2), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            i_3 = (long)(i_3 + size);
        }
        return out_2;
    }

    static double[] flatten(double[][][] maps) {
        double[] out_3 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while (i_5 < (long)(maps.length)) {
            long j_5 = 0L;
            while (j_5 < (long)(((double[][])_geto(maps, (int)((long)(i_5)))).length)) {
                long k_1 = 0L;
                while (k_1 < (long)(((double[])_geto(((double[][])_geto(maps, (int)((long)(i_5)))), (int)((long)(j_5)))).length)) {
                    out_3 = ((double[])(appendDouble(out_3, (double)(_getd(((double[])_geto(((double[][])_geto(maps, (int)((long)(i_5)))), (int)((long)(j_5)))), (int)((long)(k_1)))))));
                    k_1 = (long)(k_1 + (long)(1));
                }
                j_5 = (long)(j_5 + (long)(1));
            }
            i_5 = (long)(i_5 + (long)(1));
        }
        return out_3;
    }

    static double[] vec_mul_mat(double[] v, double[][] m) {
        long cols = (long)(((double[])_geto(m, (int)((long)(0)))).length);
        double[] res_1 = ((double[])(new double[]{}));
        long j_7 = 0L;
        while (j_7 < cols) {
            double sum_7 = 0.0;
            long i_7 = 0L;
            while (i_7 < (long)(v.length)) {
                sum_7 = (double)(sum_7) + (double)(_getd(v, (int)((long)(i_7)))) * (double)(_getd(((double[])_geto(m, (int)((long)(i_7)))), (int)((long)(j_7))));
                i_7 = (long)(i_7 + (long)(1));
            }
            res_1 = ((double[])(appendDouble(res_1, (double)(sum_7))));
            j_7 = (long)(j_7 + (long)(1));
        }
        return res_1;
    }

    static double[] matT_vec_mul(double[][] m, double[] v) {
        double[] res_2 = ((double[])(new double[]{}));
        long i_9 = 0L;
        while (i_9 < (long)(m.length)) {
            double sum_9 = 0.0;
            long j_9 = 0L;
            while (j_9 < (long)(((double[])_geto(m, (int)((long)(i_9)))).length)) {
                sum_9 = (double)(sum_9) + (double)(_getd(((double[])_geto(m, (int)((long)(i_9)))), (int)((long)(j_9)))) * (double)(_getd(v, (int)((long)(j_9))));
                j_9 = (long)(j_9 + (long)(1));
            }
            res_2 = ((double[])(appendDouble(res_2, (double)(sum_9))));
            i_9 = (long)(i_9 + (long)(1));
        }
        return res_2;
    }

    static double[] vec_add(double[] a, double[] b) {
        double[] res_3 = ((double[])(new double[]{}));
        long i_11 = 0L;
        while (i_11 < (long)(a.length)) {
            res_3 = ((double[])(appendDouble(res_3, (double)(_getd(a, (int)((long)(i_11)))) + (double)(_getd(b, (int)((long)(i_11)))))));
            i_11 = (long)(i_11 + (long)(1));
        }
        return res_3;
    }

    static double[] vec_sub(double[] a, double[] b) {
        double[] res_4 = ((double[])(new double[]{}));
        long i_13 = 0L;
        while (i_13 < (long)(a.length)) {
            res_4 = ((double[])(appendDouble(res_4, (double)(_getd(a, (int)((long)(i_13)))) - (double)(_getd(b, (int)((long)(i_13)))))));
            i_13 = (long)(i_13 + (long)(1));
        }
        return res_4;
    }

    static double[] vec_mul(double[] a, double[] b) {
        double[] res_5 = ((double[])(new double[]{}));
        long i_15 = 0L;
        while (i_15 < (long)(a.length)) {
            res_5 = ((double[])(appendDouble(res_5, (double)(_getd(a, (int)((long)(i_15)))) * (double)(_getd(b, (int)((long)(i_15)))))));
            i_15 = (long)(i_15 + (long)(1));
        }
        return res_5;
    }

    static double[] vec_map_sig(double[] v) {
        double[] res_6 = ((double[])(new double[]{}));
        long i_17 = 0L;
        while (i_17 < (long)(v.length)) {
            res_6 = ((double[])(appendDouble(res_6, (double)(sigmoid((double)(_getd(v, (int)((long)(i_17)))))))));
            i_17 = (long)(i_17 + (long)(1));
        }
        return res_6;
    }

    static CNN new_cnn() {
        double[][] k1 = ((double[][])(new double[][]{new double[]{1.0, 0.0}, new double[]{0.0, 1.0}}));
        double[][] k2_1 = ((double[][])(new double[][]{new double[]{0.0, 1.0}, new double[]{1.0, 0.0}}));
        double[][][] conv_kernels_1 = ((double[][][])(new double[][][]{k1, k2_1}));
        double[] conv_bias_1 = ((double[])(new double[]{0.0, 0.0}));
        long conv_step_1 = (long)(2);
        long pool_size_1 = (long)(2);
        long input_size_1 = (long)(2);
        long hidden_size_1 = (long)(2);
        long output_size_1 = (long)(2);
        double[][] w_hidden_1 = ((double[][])(new double[][]{}));
        long i_19 = 0L;
        while (i_19 < (long)(input_size_1)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_11 = 0L;
            while (j_11 < (long)(hidden_size_1)) {
                row_5 = ((double[])(appendDouble(row_5, (double)(random()) - 0.5)));
                j_11 = (long)(j_11 + (long)(1));
            }
            w_hidden_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(w_hidden_1), java.util.stream.Stream.of(row_5)).toArray(double[][]::new)));
            i_19 = (long)(i_19 + (long)(1));
        }
        double[][] w_out_1 = ((double[][])(new double[][]{}));
        i_19 = 0L;
        while (i_19 < (long)(hidden_size_1)) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_13 = 0L;
            while (j_13 < (long)(output_size_1)) {
                row_7 = ((double[])(appendDouble(row_7, (double)(random()) - 0.5)));
                j_13 = (long)(j_13 + (long)(1));
            }
            w_out_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(w_out_1), java.util.stream.Stream.of(row_7)).toArray(double[][]::new)));
            i_19 = (long)(i_19 + (long)(1));
        }
        double[] b_hidden_1 = ((double[])(new double[]{0.0, 0.0}));
        double[] b_out_1 = ((double[])(new double[]{0.0, 0.0}));
        return new CNN(conv_kernels_1, conv_bias_1, conv_step_1, pool_size_1, w_hidden_1, w_out_1, b_hidden_1, b_out_1, 0.2, 0.2);
    }

    static double[] forward(CNN cnn, double[][] data) {
        double[][][] maps = ((double[][][])(new double[][][]{}));
        long i_21 = 0L;
        while (i_21 < (long)(cnn.conv_kernels.length)) {
            double[][] conv_map_1 = ((double[][])(convolve(((double[][])(data)), ((double[][])(((double[][])_geto(cnn.conv_kernels, (int)((long)(i_21)))))), (long)(cnn.conv_step), _getd(cnn.conv_bias, (int)((long)(i_21))))));
            double[][] pooled_1 = ((double[][])(average_pool(((double[][])(conv_map_1)), (long)(cnn.pool_size))));
            maps = ((double[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(maps), java.util.stream.Stream.of(pooled_1)).toArray(double[][][]::new)));
            i_21 = (long)(i_21 + (long)(1));
        }
        double[] flat_1 = ((double[])(flatten(((double[][][])(maps)))));
        double[] hidden_net_1 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(flat_1)), ((double[][])(cnn.w_hidden))))), ((double[])(cnn.b_hidden)))));
        double[] hidden_out_1 = ((double[])(vec_map_sig(((double[])(hidden_net_1)))));
        double[] out_net_1 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(hidden_out_1)), ((double[][])(cnn.w_out))))), ((double[])(cnn.b_out)))));
        double[] out_5 = ((double[])(vec_map_sig(((double[])(out_net_1)))));
        return out_5;
    }

    static CNN train(CNN cnn, TrainSample[] samples, long epochs) {
        double[][] w_out_2 = ((double[][])(cnn.w_out));
        double[] b_out_3 = ((double[])(cnn.b_out));
        double[][] w_hidden_3 = ((double[][])(cnn.w_hidden));
        double[] b_hidden_3 = ((double[])(cnn.b_hidden));
        long e_1 = 0L;
        while (e_1 < epochs) {
            long s_1 = 0L;
            while (s_1 < (long)(samples.length)) {
                double[][] data_1 = ((double[][])(((TrainSample)_geto(samples, (int)((long)(s_1)))).image));
                double[] target_1 = ((double[])(((TrainSample)_geto(samples, (int)((long)(s_1)))).target));
                double[][][] maps_2 = ((double[][][])(new double[][][]{}));
                long i_23 = 0L;
                while (i_23 < (long)(cnn.conv_kernels.length)) {
                    double[][] conv_map_3 = ((double[][])(convolve(((double[][])(data_1)), ((double[][])(((double[][])_geto(cnn.conv_kernels, (int)((long)(i_23)))))), (long)(cnn.conv_step), _getd(cnn.conv_bias, (int)((long)(i_23))))));
                    double[][] pooled_3 = ((double[][])(average_pool(((double[][])(conv_map_3)), (long)(cnn.pool_size))));
                    maps_2 = ((double[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(maps_2), java.util.stream.Stream.of(pooled_3)).toArray(double[][][]::new)));
                    i_23 = (long)(i_23 + (long)(1));
                }
                double[] flat_3 = ((double[])(flatten(((double[][][])(maps_2)))));
                double[] hidden_net_3 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(flat_3)), ((double[][])(w_hidden_3))))), ((double[])(b_hidden_3)))));
                double[] hidden_out_3 = ((double[])(vec_map_sig(((double[])(hidden_net_3)))));
                double[] out_net_3 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(hidden_out_3)), ((double[][])(w_out_2))))), ((double[])(b_out_3)))));
                double[] out_7 = ((double[])(vec_map_sig(((double[])(out_net_3)))));
                double[] error_out_1 = ((double[])(vec_sub(((double[])(target_1)), ((double[])(out_7)))));
                double[] pd_out_1 = ((double[])(vec_mul(((double[])(error_out_1)), ((double[])(vec_mul(((double[])(out_7)), ((double[])(vec_sub(((double[])(new double[]{1.0, 1.0})), ((double[])(out_7)))))))))));
                double[] error_hidden_1 = ((double[])(matT_vec_mul(((double[][])(w_out_2)), ((double[])(pd_out_1)))));
                double[] pd_hidden_1 = ((double[])(vec_mul(((double[])(error_hidden_1)), ((double[])(vec_mul(((double[])(hidden_out_3)), ((double[])(vec_sub(((double[])(new double[]{1.0, 1.0})), ((double[])(hidden_out_3)))))))))));
                long j_15 = 0L;
                while (j_15 < (long)(w_out_2.length)) {
                    long k_3 = 0L;
                    while (k_3 < (long)(((double[])_geto(w_out_2, (int)((long)(j_15)))).length)) {
((double[])_geto(w_out_2, (int)((long)(j_15))))[(int)((long)(k_3))] = _getd(((double[])_geto(w_out_2, (int)((long)(j_15)))), (int)((long)(k_3))) + cnn.rate_weight * (double)(_getd(hidden_out_3, (int)((long)(j_15)))) * (double)(_getd(pd_out_1, (int)((long)(k_3))));
                        k_3 = (long)(k_3 + (long)(1));
                    }
                    j_15 = (long)(j_15 + (long)(1));
                }
                j_15 = 0L;
                while (j_15 < (long)(b_out_3.length)) {
b_out_3[(int)((long)(j_15))] = _getd(b_out_3, (int)((long)(j_15))) - cnn.rate_bias * (double)(_getd(pd_out_1, (int)((long)(j_15))));
                    j_15 = (long)(j_15 + (long)(1));
                }
                long i_h_1 = 0L;
                while (i_h_1 < (long)(w_hidden_3.length)) {
                    long j_h_1 = 0L;
                    while (j_h_1 < (long)(((double[])_geto(w_hidden_3, (int)((long)(i_h_1)))).length)) {
((double[])_geto(w_hidden_3, (int)((long)(i_h_1))))[(int)((long)(j_h_1))] = _getd(((double[])_geto(w_hidden_3, (int)((long)(i_h_1)))), (int)((long)(j_h_1))) + cnn.rate_weight * (double)(_getd(flat_3, (int)((long)(i_h_1)))) * (double)(_getd(pd_hidden_1, (int)((long)(j_h_1))));
                        j_h_1 = (long)(j_h_1 + (long)(1));
                    }
                    i_h_1 = (long)(i_h_1 + (long)(1));
                }
                j_15 = 0L;
                while (j_15 < (long)(b_hidden_3.length)) {
b_hidden_3[(int)((long)(j_15))] = _getd(b_hidden_3, (int)((long)(j_15))) - cnn.rate_bias * (double)(_getd(pd_hidden_1, (int)((long)(j_15))));
                    j_15 = (long)(j_15 + (long)(1));
                }
                s_1 = (long)(s_1 + (long)(1));
            }
            e_1 = (long)(e_1 + (long)(1));
        }
        return new CNN(cnn.conv_kernels, cnn.conv_bias, cnn.conv_step, cnn.pool_size, w_hidden_3, w_out_2, b_hidden_3, b_out_3, cnn.rate_weight, cnn.rate_bias);
    }

    static void main() {
        CNN cnn = new_cnn();
        double[][] image_1 = ((double[][])(new double[][]{new double[]{1.0, 0.0, 1.0, 0.0}, new double[]{0.0, 1.0, 0.0, 1.0}, new double[]{1.0, 0.0, 1.0, 0.0}, new double[]{0.0, 1.0, 0.0, 1.0}}));
        TrainSample sample_1 = new TrainSample(image_1, new double[]{1.0, 0.0});
        System.out.println("Before training:" + " " + String.valueOf(forward(cnn, ((double[][])(image_1)))));
        CNN trained_1 = train(cnn, ((TrainSample[])(new TrainSample[]{sample_1})), 50L);
        System.out.println("After training:" + " " + String.valueOf(forward(trained_1, ((double[][])(image_1)))));
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
