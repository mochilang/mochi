public class Main {
    static class CNN {
        double[][][] conv_kernels;
        double[] conv_bias;
        int conv_step;
        int pool_size;
        double[][] w_hidden;
        double[][] w_out;
        double[] b_hidden;
        double[] b_out;
        double rate_weight;
        double rate_bias;
        CNN(double[][][] conv_kernels, double[] conv_bias, int conv_step, int pool_size, double[][] w_hidden, double[][] w_out, double[] b_hidden, double[] b_out, double rate_weight, double rate_bias) {
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

    static int seed = 0;
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
        seed = Math.floorMod((seed * 13 + 7), 100);
        return (((Number)(seed)).doubleValue()) / 100.0;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + exp(-x));
    }

    static double to_float(int x) {
        return x * 1.0;
    }

    static double exp(double x) {
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 20) {
            term = term * x / to_float(n);
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double[][] convolve(double[][] data, double[][] kernel, int step, double bias) {
        int size_data = data.length;
        int size_kernel = kernel.length;
        double[][] out = ((double[][])(new double[][]{}));
        int i = 0;
        while (i <= size_data - size_kernel) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j <= size_data - size_kernel) {
                double sum_1 = 0.0;
                int a = 0;
                while (a < size_kernel) {
                    int b = 0;
                    while (b < size_kernel) {
                        sum_1 = sum_1 + data[i + a][j + b] * kernel[a][b];
                        b = b + 1;
                    }
                    a = a + 1;
                }
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(sigmoid(sum_1 - bias))).toArray()));
                j = j + step;
            }
            out = ((double[][])(appendObj(out, row)));
            i = i + step;
        }
        return out;
    }

    static double[][] average_pool(double[][] map, int size) {
        double[][] out_1 = ((double[][])(new double[][]{}));
        int i_1 = 0;
        while (i_1 < map.length) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < map[i_1].length) {
                double sum_2 = 0.0;
                int a_1 = 0;
                while (a_1 < size) {
                    int b_1 = 0;
                    while (b_1 < size) {
                        sum_2 = sum_2 + map[i_1 + a_1][j_1 + b_1];
                        b_1 = b_1 + 1;
                    }
                    a_1 = a_1 + 1;
                }
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(sum_2 / (((Number)((size * size))).doubleValue()))).toArray()));
                j_1 = j_1 + size;
            }
            out_1 = ((double[][])(appendObj(out_1, row_1)));
            i_1 = i_1 + size;
        }
        return out_1;
    }

    static double[] flatten(double[][][] maps) {
        double[] out_2 = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < maps.length) {
            int j_2 = 0;
            while (j_2 < maps[i_2].length) {
                int k = 0;
                while (k < maps[i_2][j_2].length) {
                    out_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(out_2), java.util.stream.DoubleStream.of(maps[i_2][j_2][k])).toArray()));
                    k = k + 1;
                }
                j_2 = j_2 + 1;
            }
            i_2 = i_2 + 1;
        }
        return out_2;
    }

    static double[] vec_mul_mat(double[] v, double[][] m) {
        int cols = m[0].length;
        double[] res = ((double[])(new double[]{}));
        int j_3 = 0;
        while (j_3 < cols) {
            double sum_3 = 0.0;
            int i_3 = 0;
            while (i_3 < v.length) {
                sum_3 = sum_3 + v[i_3] * m[i_3][j_3];
                i_3 = i_3 + 1;
            }
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(sum_3)).toArray()));
            j_3 = j_3 + 1;
        }
        return res;
    }

    static double[] matT_vec_mul(double[][] m, double[] v) {
        double[] res_1 = ((double[])(new double[]{}));
        int i_4 = 0;
        while (i_4 < m.length) {
            double sum_4 = 0.0;
            int j_4 = 0;
            while (j_4 < m[i_4].length) {
                sum_4 = sum_4 + m[i_4][j_4] * v[j_4];
                j_4 = j_4 + 1;
            }
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(sum_4)).toArray()));
            i_4 = i_4 + 1;
        }
        return res_1;
    }

    static double[] vec_add(double[] a, double[] b) {
        double[] res_2 = ((double[])(new double[]{}));
        int i_5 = 0;
        while (i_5 < a.length) {
            res_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_2), java.util.stream.DoubleStream.of(a[i_5] + b[i_5])).toArray()));
            i_5 = i_5 + 1;
        }
        return res_2;
    }

    static double[] vec_sub(double[] a, double[] b) {
        double[] res_3 = ((double[])(new double[]{}));
        int i_6 = 0;
        while (i_6 < a.length) {
            res_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_3), java.util.stream.DoubleStream.of(a[i_6] - b[i_6])).toArray()));
            i_6 = i_6 + 1;
        }
        return res_3;
    }

    static double[] vec_mul(double[] a, double[] b) {
        double[] res_4 = ((double[])(new double[]{}));
        int i_7 = 0;
        while (i_7 < a.length) {
            res_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_4), java.util.stream.DoubleStream.of(a[i_7] * b[i_7])).toArray()));
            i_7 = i_7 + 1;
        }
        return res_4;
    }

    static double[] vec_map_sig(double[] v) {
        double[] res_5 = ((double[])(new double[]{}));
        int i_8 = 0;
        while (i_8 < v.length) {
            res_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_5), java.util.stream.DoubleStream.of(sigmoid(v[i_8]))).toArray()));
            i_8 = i_8 + 1;
        }
        return res_5;
    }

    static CNN new_cnn() {
        double[][] k1 = ((double[][])(new double[][]{new double[]{1.0, 0.0}, new double[]{0.0, 1.0}}));
        double[][] k2 = ((double[][])(new double[][]{new double[]{0.0, 1.0}, new double[]{1.0, 0.0}}));
        double[][][] conv_kernels = ((double[][][])(new double[][][]{k1, k2}));
        double[] conv_bias = ((double[])(new double[]{0.0, 0.0}));
        int conv_step = 2;
        int pool_size = 2;
        int input_size = 2;
        int hidden_size = 2;
        int output_size = 2;
        double[][] w_hidden = ((double[][])(new double[][]{}));
        int i_9 = 0;
        while (i_9 < input_size) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_5 = 0;
            while (j_5 < hidden_size) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(random() - 0.5)).toArray()));
                j_5 = j_5 + 1;
            }
            w_hidden = ((double[][])(appendObj(w_hidden, row_2)));
            i_9 = i_9 + 1;
        }
        double[][] w_out = ((double[][])(new double[][]{}));
        i_9 = 0;
        while (i_9 < hidden_size) {
            double[] row_3 = ((double[])(new double[]{}));
            int j_6 = 0;
            while (j_6 < output_size) {
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(random() - 0.5)).toArray()));
                j_6 = j_6 + 1;
            }
            w_out = ((double[][])(appendObj(w_out, row_3)));
            i_9 = i_9 + 1;
        }
        double[] b_hidden = ((double[])(new double[]{0.0, 0.0}));
        double[] b_out = ((double[])(new double[]{0.0, 0.0}));
        return new CNN(conv_kernels, conv_bias, conv_step, pool_size, w_hidden, w_out, b_hidden, b_out, 0.2, 0.2);
    }

    static double[] forward(CNN cnn, double[][] data) {
        double[][][] maps = ((double[][][])(new double[][][]{}));
        int i_10 = 0;
        while (i_10 < cnn.conv_kernels.length) {
            double[][] conv_map = ((double[][])(convolve(((double[][])(data)), ((double[][])(cnn.conv_kernels[i_10])), cnn.conv_step, cnn.conv_bias[i_10])));
            double[][] pooled = ((double[][])(average_pool(((double[][])(conv_map)), cnn.pool_size)));
            maps = ((double[][][])(appendObj(maps, pooled)));
            i_10 = i_10 + 1;
        }
        double[] flat = ((double[])(flatten(((double[][][])(maps)))));
        double[] hidden_net = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(flat)), ((double[][])(cnn.w_hidden))))), ((double[])(cnn.b_hidden)))));
        double[] hidden_out = ((double[])(vec_map_sig(((double[])(hidden_net)))));
        double[] out_net = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(hidden_out)), ((double[][])(cnn.w_out))))), ((double[])(cnn.b_out)))));
        double[] out_3 = ((double[])(vec_map_sig(((double[])(out_net)))));
        return out_3;
    }

    static CNN train(CNN cnn, TrainSample[] samples, int epochs) {
        double[][] w_out_1 = ((double[][])(cnn.w_out));
        double[] b_out_1 = ((double[])(cnn.b_out));
        double[][] w_hidden_1 = ((double[][])(cnn.w_hidden));
        double[] b_hidden_1 = ((double[])(cnn.b_hidden));
        int e = 0;
        while (e < epochs) {
            int s = 0;
            while (s < samples.length) {
                double[][] data = ((double[][])(samples[s].image));
                double[] target = ((double[])(samples[s].target));
                double[][][] maps_1 = ((double[][][])(new double[][][]{}));
                int i_11 = 0;
                while (i_11 < cnn.conv_kernels.length) {
                    double[][] conv_map_1 = ((double[][])(convolve(((double[][])(data)), ((double[][])(cnn.conv_kernels[i_11])), cnn.conv_step, cnn.conv_bias[i_11])));
                    double[][] pooled_1 = ((double[][])(average_pool(((double[][])(conv_map_1)), cnn.pool_size)));
                    maps_1 = ((double[][][])(appendObj(maps_1, pooled_1)));
                    i_11 = i_11 + 1;
                }
                double[] flat_1 = ((double[])(flatten(((double[][][])(maps_1)))));
                double[] hidden_net_1 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(flat_1)), ((double[][])(w_hidden_1))))), ((double[])(b_hidden_1)))));
                double[] hidden_out_1 = ((double[])(vec_map_sig(((double[])(hidden_net_1)))));
                double[] out_net_1 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(hidden_out_1)), ((double[][])(w_out_1))))), ((double[])(b_out_1)))));
                double[] out_4 = ((double[])(vec_map_sig(((double[])(out_net_1)))));
                double[] error_out = ((double[])(vec_sub(((double[])(target)), ((double[])(out_4)))));
                double[] pd_out = ((double[])(vec_mul(((double[])(error_out)), ((double[])(vec_mul(((double[])(out_4)), ((double[])(vec_sub(((double[])(new double[]{1.0, 1.0})), ((double[])(out_4)))))))))));
                double[] error_hidden = ((double[])(matT_vec_mul(((double[][])(w_out_1)), ((double[])(pd_out)))));
                double[] pd_hidden = ((double[])(vec_mul(((double[])(error_hidden)), ((double[])(vec_mul(((double[])(hidden_out_1)), ((double[])(vec_sub(((double[])(new double[]{1.0, 1.0})), ((double[])(hidden_out_1)))))))))));
                int j_7 = 0;
                while (j_7 < w_out_1.length) {
                    int k_1 = 0;
                    while (k_1 < w_out_1[j_7].length) {
w_out_1[j_7][k_1] = w_out_1[j_7][k_1] + cnn.rate_weight * hidden_out_1[j_7] * pd_out[k_1];
                        k_1 = k_1 + 1;
                    }
                    j_7 = j_7 + 1;
                }
                j_7 = 0;
                while (j_7 < b_out_1.length) {
b_out_1[j_7] = b_out_1[j_7] - cnn.rate_bias * pd_out[j_7];
                    j_7 = j_7 + 1;
                }
                int i_h = 0;
                while (i_h < w_hidden_1.length) {
                    int j_h = 0;
                    while (j_h < w_hidden_1[i_h].length) {
w_hidden_1[i_h][j_h] = w_hidden_1[i_h][j_h] + cnn.rate_weight * flat_1[i_h] * pd_hidden[j_h];
                        j_h = j_h + 1;
                    }
                    i_h = i_h + 1;
                }
                j_7 = 0;
                while (j_7 < b_hidden_1.length) {
b_hidden_1[j_7] = b_hidden_1[j_7] - cnn.rate_bias * pd_hidden[j_7];
                    j_7 = j_7 + 1;
                }
                s = s + 1;
            }
            e = e + 1;
        }
        return new CNN(cnn.conv_kernels, cnn.conv_bias, cnn.conv_step, cnn.pool_size, w_hidden_1, w_out_1, b_hidden_1, b_out_1, cnn.rate_weight, cnn.rate_bias);
    }

    static void main() {
        CNN cnn = new_cnn();
        double[][] image = ((double[][])(new double[][]{new double[]{1.0, 0.0, 1.0, 0.0}, new double[]{0.0, 1.0, 0.0, 1.0}, new double[]{1.0, 0.0, 1.0, 0.0}, new double[]{0.0, 1.0, 0.0, 1.0}}));
        TrainSample sample = new TrainSample(image, new double[]{1.0, 0.0});
        System.out.println("Before training:" + " " + String.valueOf(forward(cnn, ((double[][])(image)))));
        CNN trained = train(cnn, ((TrainSample[])(new TrainSample[]{sample})), 50);
        System.out.println("After training:" + " " + String.valueOf(forward(trained, ((double[][])(image)))));
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
