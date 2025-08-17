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

    static long seed = 1L;
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
        seed = Math.floorMod(((long)((long)(seed) * 13L) + 7L), 100);
        return (double)((((Number)(seed)).doubleValue())) / (double)(100.0);
    }

    static double sigmoid(double x) {
        return (double)(1.0) / (double)(((double)(1.0) + (double)(exp((double)(-x)))));
    }

    static double to_float(long x) {
        return (double)(x) * (double)(1.0);
    }

    static double exp(double x) {
        double term = (double)(1.0);
        double sum_1 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 20L) {
            term = (double)((double)((double)(term) * (double)(x)) / (double)(((Number)(n_1)).doubleValue()));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return sum_1;
    }

    static double[][] convolve(double[][] data, double[][] kernel, long step, double bias) {
        long size_data = (long)(data.length);
        long size_kernel_1 = (long)(kernel.length);
        double[][] out_1 = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= (long)((long)(size_data) - (long)(size_kernel_1))) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while ((long)(j_1) <= (long)((long)(size_data) - (long)(size_kernel_1))) {
                double sum_3 = (double)(0.0);
                long a_1 = 0L;
                while ((long)(a_1) < (long)(size_kernel_1)) {
                    long b_1 = 0L;
                    while ((long)(b_1) < (long)(size_kernel_1)) {
                        sum_3 = (double)((double)(sum_3) + (double)((double)(data[(int)((long)((long)(i_1) + (long)(a_1)))][(int)((long)((long)(j_1) + (long)(b_1)))]) * (double)(kernel[(int)((long)(a_1))][(int)((long)(b_1))])));
                        b_1 = (long)((long)(b_1) + 1L);
                    }
                    a_1 = (long)((long)(a_1) + 1L);
                }
                row_1 = ((double[])(appendDouble(row_1, (double)(sigmoid((double)((double)(sum_3) - (double)(bias)))))));
                j_1 = (long)((long)(j_1) + (long)(step));
            }
            out_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(out_1), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            i_1 = (long)((long)(i_1) + (long)(step));
        }
        return out_1;
    }

    static double[][] average_pool(double[][] map, long size) {
        double[][] out_2 = ((double[][])(new double[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(map.length)) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(map[(int)((long)(i_3))].length)) {
                double sum_5 = (double)(0.0);
                long a_3 = 0L;
                while ((long)(a_3) < (long)(size)) {
                    long b_3 = 0L;
                    while ((long)(b_3) < (long)(size)) {
                        sum_5 = (double)((double)(sum_5) + (double)(map[(int)((long)((long)(i_3) + (long)(a_3)))][(int)((long)((long)(j_3) + (long)(b_3)))]));
                        b_3 = (long)((long)(b_3) + 1L);
                    }
                    a_3 = (long)((long)(a_3) + 1L);
                }
                row_3 = ((double[])(appendDouble(row_3, (double)((double)(sum_5) / (double)((((Number)(((long)(size) * (long)(size)))).doubleValue()))))));
                j_3 = (long)((long)(j_3) + (long)(size));
            }
            out_2 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(out_2), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
            i_3 = (long)((long)(i_3) + (long)(size));
        }
        return out_2;
    }

    static double[] flatten(double[][][] maps) {
        double[] out_3 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(maps.length)) {
            long j_5 = 0L;
            while ((long)(j_5) < (long)(maps[(int)((long)(i_5))].length)) {
                long k_1 = 0L;
                while ((long)(k_1) < (long)(maps[(int)((long)(i_5))][(int)((long)(j_5))].length)) {
                    out_3 = ((double[])(appendDouble(out_3, (double)(maps[(int)((long)(i_5))][(int)((long)(j_5))][(int)((long)(k_1))]))));
                    k_1 = (long)((long)(k_1) + 1L);
                }
                j_5 = (long)((long)(j_5) + 1L);
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        return out_3;
    }

    static double[] vec_mul_mat(double[] v, double[][] m) {
        long cols = (long)(m[(int)(0L)].length);
        double[] res_1 = ((double[])(new double[]{}));
        long j_7 = 0L;
        while ((long)(j_7) < (long)(cols)) {
            double sum_7 = (double)(0.0);
            long i_7 = 0L;
            while ((long)(i_7) < (long)(v.length)) {
                sum_7 = (double)((double)(sum_7) + (double)((double)(v[(int)((long)(i_7))]) * (double)(m[(int)((long)(i_7))][(int)((long)(j_7))])));
                i_7 = (long)((long)(i_7) + 1L);
            }
            res_1 = ((double[])(appendDouble(res_1, (double)(sum_7))));
            j_7 = (long)((long)(j_7) + 1L);
        }
        return res_1;
    }

    static double[] matT_vec_mul(double[][] m, double[] v) {
        double[] res_2 = ((double[])(new double[]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(m.length)) {
            double sum_9 = (double)(0.0);
            long j_9 = 0L;
            while ((long)(j_9) < (long)(m[(int)((long)(i_9))].length)) {
                sum_9 = (double)((double)(sum_9) + (double)((double)(m[(int)((long)(i_9))][(int)((long)(j_9))]) * (double)(v[(int)((long)(j_9))])));
                j_9 = (long)((long)(j_9) + 1L);
            }
            res_2 = ((double[])(appendDouble(res_2, (double)(sum_9))));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return res_2;
    }

    static double[] vec_add(double[] a, double[] b) {
        double[] res_3 = ((double[])(new double[]{}));
        long i_11 = 0L;
        while ((long)(i_11) < (long)(a.length)) {
            res_3 = ((double[])(appendDouble(res_3, (double)((double)(a[(int)((long)(i_11))]) + (double)(b[(int)((long)(i_11))])))));
            i_11 = (long)((long)(i_11) + 1L);
        }
        return res_3;
    }

    static double[] vec_sub(double[] a, double[] b) {
        double[] res_4 = ((double[])(new double[]{}));
        long i_13 = 0L;
        while ((long)(i_13) < (long)(a.length)) {
            res_4 = ((double[])(appendDouble(res_4, (double)((double)(a[(int)((long)(i_13))]) - (double)(b[(int)((long)(i_13))])))));
            i_13 = (long)((long)(i_13) + 1L);
        }
        return res_4;
    }

    static double[] vec_mul(double[] a, double[] b) {
        double[] res_5 = ((double[])(new double[]{}));
        long i_15 = 0L;
        while ((long)(i_15) < (long)(a.length)) {
            res_5 = ((double[])(appendDouble(res_5, (double)((double)(a[(int)((long)(i_15))]) * (double)(b[(int)((long)(i_15))])))));
            i_15 = (long)((long)(i_15) + 1L);
        }
        return res_5;
    }

    static double[] vec_map_sig(double[] v) {
        double[] res_6 = ((double[])(new double[]{}));
        long i_17 = 0L;
        while ((long)(i_17) < (long)(v.length)) {
            res_6 = ((double[])(appendDouble(res_6, (double)(sigmoid((double)(v[(int)((long)(i_17))]))))));
            i_17 = (long)((long)(i_17) + 1L);
        }
        return res_6;
    }

    static CNN new_cnn() {
        double[][] k1 = ((double[][])(new double[][]{new double[]{1.0, 0.0}, new double[]{0.0, 1.0}}));
        double[][] k2_1 = ((double[][])(new double[][]{new double[]{0.0, 1.0}, new double[]{1.0, 0.0}}));
        double[][][] conv_kernels_1 = ((double[][][])(new double[][][]{k1, k2_1}));
        double[] conv_bias_1 = ((double[])(new double[]{0.0, 0.0}));
        long conv_step_1 = 2L;
        long pool_size_1 = 2L;
        long input_size_1 = 2L;
        long hidden_size_1 = 2L;
        long output_size_1 = 2L;
        double[][] w_hidden_1 = ((double[][])(new double[][]{}));
        long i_19 = 0L;
        while ((long)(i_19) < (long)(input_size_1)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_11 = 0L;
            while ((long)(j_11) < (long)(hidden_size_1)) {
                row_5 = ((double[])(appendDouble(row_5, (double)((double)(random()) - (double)(0.5)))));
                j_11 = (long)((long)(j_11) + 1L);
            }
            w_hidden_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(w_hidden_1), java.util.stream.Stream.of(new double[][]{row_5})).toArray(double[][]::new)));
            i_19 = (long)((long)(i_19) + 1L);
        }
        double[][] w_out_1 = ((double[][])(new double[][]{}));
        i_19 = 0L;
        while ((long)(i_19) < (long)(hidden_size_1)) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_13 = 0L;
            while ((long)(j_13) < (long)(output_size_1)) {
                row_7 = ((double[])(appendDouble(row_7, (double)((double)(random()) - (double)(0.5)))));
                j_13 = (long)((long)(j_13) + 1L);
            }
            w_out_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(w_out_1), java.util.stream.Stream.of(new double[][]{row_7})).toArray(double[][]::new)));
            i_19 = (long)((long)(i_19) + 1L);
        }
        double[] b_hidden_1 = ((double[])(new double[]{0.0, 0.0}));
        double[] b_out_1 = ((double[])(new double[]{0.0, 0.0}));
        return new CNN(conv_kernels_1, conv_bias_1, conv_step_1, pool_size_1, w_hidden_1, w_out_1, b_hidden_1, b_out_1, 0.2, 0.2);
    }

    static double[] forward(CNN cnn, double[][] data) {
        double[][][] maps = ((double[][][])(new double[][][]{}));
        long i_21 = 0L;
        while ((long)(i_21) < (long)(cnn.conv_kernels.length)) {
            double[][] conv_map_1 = ((double[][])(convolve(((double[][])(data)), ((double[][])(cnn.conv_kernels[(int)((long)(i_21))])), (long)(cnn.conv_step), (double)(cnn.conv_bias[(int)((long)(i_21))]))));
            double[][] pooled_1 = ((double[][])(average_pool(((double[][])(conv_map_1)), (long)(cnn.pool_size))));
            maps = ((double[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(maps), java.util.stream.Stream.of(new double[][][]{pooled_1})).toArray(double[][][]::new)));
            i_21 = (long)((long)(i_21) + 1L);
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
        while ((long)(e_1) < (long)(epochs)) {
            long s_1 = 0L;
            while ((long)(s_1) < (long)(samples.length)) {
                double[][] data_1 = ((double[][])(samples[(int)((long)(s_1))].image));
                double[] target_1 = ((double[])(samples[(int)((long)(s_1))].target));
                double[][][] maps_2 = ((double[][][])(new double[][][]{}));
                long i_23 = 0L;
                while ((long)(i_23) < (long)(cnn.conv_kernels.length)) {
                    double[][] conv_map_3 = ((double[][])(convolve(((double[][])(data_1)), ((double[][])(cnn.conv_kernels[(int)((long)(i_23))])), (long)(cnn.conv_step), (double)(cnn.conv_bias[(int)((long)(i_23))]))));
                    double[][] pooled_3 = ((double[][])(average_pool(((double[][])(conv_map_3)), (long)(cnn.pool_size))));
                    maps_2 = ((double[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(maps_2), java.util.stream.Stream.of(new double[][][]{pooled_3})).toArray(double[][][]::new)));
                    i_23 = (long)((long)(i_23) + 1L);
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
                while ((long)(j_15) < (long)(w_out_2.length)) {
                    long k_3 = 0L;
                    while ((long)(k_3) < (long)(w_out_2[(int)((long)(j_15))].length)) {
w_out_2[(int)((long)(j_15))][(int)((long)(k_3))] = (double)((double)(w_out_2[(int)((long)(j_15))][(int)((long)(k_3))]) + (double)((double)((double)(cnn.rate_weight) * (double)(hidden_out_3[(int)((long)(j_15))])) * (double)(pd_out_1[(int)((long)(k_3))])));
                        k_3 = (long)((long)(k_3) + 1L);
                    }
                    j_15 = (long)((long)(j_15) + 1L);
                }
                j_15 = 0L;
                while ((long)(j_15) < (long)(b_out_3.length)) {
b_out_3[(int)((long)(j_15))] = (double)((double)(b_out_3[(int)((long)(j_15))]) - (double)((double)(cnn.rate_bias) * (double)(pd_out_1[(int)((long)(j_15))])));
                    j_15 = (long)((long)(j_15) + 1L);
                }
                long i_h_1 = 0L;
                while ((long)(i_h_1) < (long)(w_hidden_3.length)) {
                    long j_h_1 = 0L;
                    while ((long)(j_h_1) < (long)(w_hidden_3[(int)((long)(i_h_1))].length)) {
w_hidden_3[(int)((long)(i_h_1))][(int)((long)(j_h_1))] = (double)((double)(w_hidden_3[(int)((long)(i_h_1))][(int)((long)(j_h_1))]) + (double)((double)((double)(cnn.rate_weight) * (double)(flat_3[(int)((long)(i_h_1))])) * (double)(pd_hidden_1[(int)((long)(j_h_1))])));
                        j_h_1 = (long)((long)(j_h_1) + 1L);
                    }
                    i_h_1 = (long)((long)(i_h_1) + 1L);
                }
                j_15 = 0L;
                while ((long)(j_15) < (long)(b_hidden_3.length)) {
b_hidden_3[(int)((long)(j_15))] = (double)((double)(b_hidden_3[(int)((long)(j_15))]) - (double)((double)(cnn.rate_bias) * (double)(pd_hidden_1[(int)((long)(j_15))])));
                    j_15 = (long)((long)(j_15) + 1L);
                }
                s_1 = (long)((long)(s_1) + 1L);
            }
            e_1 = (long)((long)(e_1) + 1L);
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
}
