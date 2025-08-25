public class Main {
    static class CNN {
        double[][][] conv_kernels;
        double[] conv_bias;
        java.math.BigInteger conv_step;
        java.math.BigInteger pool_size;
        double[][] w_hidden;
        double[][] w_out;
        double[] b_hidden;
        double[] b_out;
        double rate_weight;
        double rate_bias;
        CNN(double[][][] conv_kernels, double[] conv_bias, java.math.BigInteger conv_step, java.math.BigInteger pool_size, double[][] w_hidden, double[][] w_out, double[] b_hidden, double[] b_out, double rate_weight, double rate_bias) {
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

    static java.math.BigInteger seed = java.math.BigInteger.valueOf(1);
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
        seed = new java.math.BigInteger(String.valueOf((seed.multiply(java.math.BigInteger.valueOf(13)).add(java.math.BigInteger.valueOf(7))).remainder(java.math.BigInteger.valueOf(100))));
        return (double)((double)((((Number)(seed)).doubleValue())) / (double)(100.0));
    }

    static double sigmoid(double x) {
        return (double)((double)(1.0) / (double)(((double)(1.0) + (double)(Math.exp(-x)))));
    }

    static double to_float(java.math.BigInteger x) {
        return (double)(((java.math.BigInteger)(x)).doubleValue() * (double)(1.0));
    }

    static double exp(double x) {
        double term = (double)(1.0);
        double sum_1 = (double)(1.0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(1);
        while (n_1.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            term = (double)((double)((double)(term) * (double)(x)) / (double)(((Number)(n_1)).doubleValue()));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            n_1 = new java.math.BigInteger(String.valueOf(n_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(sum_1);
    }

    static double[][] convolve(double[][] data, double[][] kernel, java.math.BigInteger step, double bias) {
        java.math.BigInteger size_data = new java.math.BigInteger(String.valueOf(data.length));
        java.math.BigInteger size_kernel_1 = new java.math.BigInteger(String.valueOf(kernel.length));
        double[][] out_1 = ((double[][])(new double[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(size_data.subtract(size_kernel_1)) <= 0) {
            double[] row_1 = ((double[])(new double[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(size_data.subtract(size_kernel_1)) <= 0) {
                double sum_3 = (double)(0.0);
                java.math.BigInteger a_1 = java.math.BigInteger.valueOf(0);
                while (a_1.compareTo(size_kernel_1) < 0) {
                    java.math.BigInteger b_1 = java.math.BigInteger.valueOf(0);
                    while (b_1.compareTo(size_kernel_1) < 0) {
                        sum_3 = (double)((double)(sum_3) + (double)((double)(data[_idx((data).length, ((java.math.BigInteger)(i_1.add(a_1))).longValue())][_idx((data[_idx((data).length, ((java.math.BigInteger)(i_1.add(a_1))).longValue())]).length, ((java.math.BigInteger)(j_1.add(b_1))).longValue())]) * (double)(kernel[_idx((kernel).length, ((java.math.BigInteger)(a_1)).longValue())][_idx((kernel[_idx((kernel).length, ((java.math.BigInteger)(a_1)).longValue())]).length, ((java.math.BigInteger)(b_1)).longValue())])));
                        b_1 = new java.math.BigInteger(String.valueOf(b_1.add(java.math.BigInteger.valueOf(1))));
                    }
                    a_1 = new java.math.BigInteger(String.valueOf(a_1.add(java.math.BigInteger.valueOf(1))));
                }
                row_1 = ((double[])(appendDouble(row_1, (double)(sigmoid((double)((double)(sum_3) - (double)(bias)))))));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(step)));
            }
            out_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(out_1), java.util.stream.Stream.of(new double[][]{((double[])(row_1))})).toArray(double[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(step)));
        }
        return ((double[][])(out_1));
    }

    static double[][] average_pool(double[][] map, java.math.BigInteger size) {
        double[][] out_2 = ((double[][])(new double[][]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(map.length))) < 0) {
            double[] row_3 = ((double[])(new double[]{}));
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
            while (j_3.compareTo(new java.math.BigInteger(String.valueOf(map[_idx((map).length, ((java.math.BigInteger)(i_3)).longValue())].length))) < 0) {
                double sum_5 = (double)(0.0);
                java.math.BigInteger a_3 = java.math.BigInteger.valueOf(0);
                while (a_3.compareTo(size) < 0) {
                    java.math.BigInteger b_3 = java.math.BigInteger.valueOf(0);
                    while (b_3.compareTo(size) < 0) {
                        sum_5 = (double)((double)(sum_5) + (double)(map[_idx((map).length, ((java.math.BigInteger)(i_3.add(a_3))).longValue())][_idx((map[_idx((map).length, ((java.math.BigInteger)(i_3.add(a_3))).longValue())]).length, ((java.math.BigInteger)(j_3.add(b_3))).longValue())]));
                        b_3 = new java.math.BigInteger(String.valueOf(b_3.add(java.math.BigInteger.valueOf(1))));
                    }
                    a_3 = new java.math.BigInteger(String.valueOf(a_3.add(java.math.BigInteger.valueOf(1))));
                }
                row_3 = ((double[])(appendDouble(row_3, (double)((double)(sum_5) / (double)((((Number)((size.multiply(size)))).doubleValue()))))));
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(size)));
            }
            out_2 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(out_2), java.util.stream.Stream.of(new double[][]{((double[])(row_3))})).toArray(double[][]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(size)));
        }
        return ((double[][])(out_2));
    }

    static double[] flatten(double[][][] maps) {
        double[] out_3 = ((double[])(new double[]{}));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(maps.length))) < 0) {
            java.math.BigInteger j_5 = java.math.BigInteger.valueOf(0);
            while (j_5.compareTo(new java.math.BigInteger(String.valueOf(maps[_idx((maps).length, ((java.math.BigInteger)(i_5)).longValue())].length))) < 0) {
                java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
                while (k_1.compareTo(new java.math.BigInteger(String.valueOf(maps[_idx((maps).length, ((java.math.BigInteger)(i_5)).longValue())][_idx((maps[_idx((maps).length, ((java.math.BigInteger)(i_5)).longValue())]).length, ((java.math.BigInteger)(j_5)).longValue())].length))) < 0) {
                    out_3 = ((double[])(appendDouble(out_3, (double)(maps[_idx((maps).length, ((java.math.BigInteger)(i_5)).longValue())][_idx((maps[_idx((maps).length, ((java.math.BigInteger)(i_5)).longValue())]).length, ((java.math.BigInteger)(j_5)).longValue())][_idx((maps[_idx((maps).length, ((java.math.BigInteger)(i_5)).longValue())][_idx((maps[_idx((maps).length, ((java.math.BigInteger)(i_5)).longValue())]).length, ((java.math.BigInteger)(j_5)).longValue())]).length, ((java.math.BigInteger)(k_1)).longValue())]))));
                    k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
                }
                j_5 = new java.math.BigInteger(String.valueOf(j_5.add(java.math.BigInteger.valueOf(1))));
            }
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(out_3));
    }

    static double[] vec_mul_mat(double[] v, double[][] m) {
        java.math.BigInteger cols = new java.math.BigInteger(String.valueOf(m[_idx((m).length, 0L)].length));
        double[] res_1 = ((double[])(new double[]{}));
        java.math.BigInteger j_7 = java.math.BigInteger.valueOf(0);
        while (j_7.compareTo(cols) < 0) {
            double sum_7 = (double)(0.0);
            java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
            while (i_7.compareTo(new java.math.BigInteger(String.valueOf(v.length))) < 0) {
                sum_7 = (double)((double)(sum_7) + (double)((double)(v[_idx((v).length, ((java.math.BigInteger)(i_7)).longValue())]) * (double)(m[_idx((m).length, ((java.math.BigInteger)(i_7)).longValue())][_idx((m[_idx((m).length, ((java.math.BigInteger)(i_7)).longValue())]).length, ((java.math.BigInteger)(j_7)).longValue())])));
                i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
            }
            res_1 = ((double[])(appendDouble(res_1, (double)(sum_7))));
            j_7 = new java.math.BigInteger(String.valueOf(j_7.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_1));
    }

    static double[] matT_vec_mul(double[][] m, double[] v) {
        double[] res_2 = ((double[])(new double[]{}));
        java.math.BigInteger i_9 = java.math.BigInteger.valueOf(0);
        while (i_9.compareTo(new java.math.BigInteger(String.valueOf(m.length))) < 0) {
            double sum_9 = (double)(0.0);
            java.math.BigInteger j_9 = java.math.BigInteger.valueOf(0);
            while (j_9.compareTo(new java.math.BigInteger(String.valueOf(m[_idx((m).length, ((java.math.BigInteger)(i_9)).longValue())].length))) < 0) {
                sum_9 = (double)((double)(sum_9) + (double)((double)(m[_idx((m).length, ((java.math.BigInteger)(i_9)).longValue())][_idx((m[_idx((m).length, ((java.math.BigInteger)(i_9)).longValue())]).length, ((java.math.BigInteger)(j_9)).longValue())]) * (double)(v[_idx((v).length, ((java.math.BigInteger)(j_9)).longValue())])));
                j_9 = new java.math.BigInteger(String.valueOf(j_9.add(java.math.BigInteger.valueOf(1))));
            }
            res_2 = ((double[])(appendDouble(res_2, (double)(sum_9))));
            i_9 = new java.math.BigInteger(String.valueOf(i_9.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_2));
    }

    static double[] vec_add(double[] a, double[] b) {
        double[] res_3 = ((double[])(new double[]{}));
        java.math.BigInteger i_11 = java.math.BigInteger.valueOf(0);
        while (i_11.compareTo(new java.math.BigInteger(String.valueOf(a.length))) < 0) {
            res_3 = ((double[])(appendDouble(res_3, (double)((double)(a[_idx((a).length, ((java.math.BigInteger)(i_11)).longValue())]) + (double)(b[_idx((b).length, ((java.math.BigInteger)(i_11)).longValue())])))));
            i_11 = new java.math.BigInteger(String.valueOf(i_11.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_3));
    }

    static double[] vec_sub(double[] a, double[] b) {
        double[] res_4 = ((double[])(new double[]{}));
        java.math.BigInteger i_13 = java.math.BigInteger.valueOf(0);
        while (i_13.compareTo(new java.math.BigInteger(String.valueOf(a.length))) < 0) {
            res_4 = ((double[])(appendDouble(res_4, (double)((double)(a[_idx((a).length, ((java.math.BigInteger)(i_13)).longValue())]) - (double)(b[_idx((b).length, ((java.math.BigInteger)(i_13)).longValue())])))));
            i_13 = new java.math.BigInteger(String.valueOf(i_13.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_4));
    }

    static double[] vec_mul(double[] a, double[] b) {
        double[] res_5 = ((double[])(new double[]{}));
        java.math.BigInteger i_15 = java.math.BigInteger.valueOf(0);
        while (i_15.compareTo(new java.math.BigInteger(String.valueOf(a.length))) < 0) {
            res_5 = ((double[])(appendDouble(res_5, (double)((double)(a[_idx((a).length, ((java.math.BigInteger)(i_15)).longValue())]) * (double)(b[_idx((b).length, ((java.math.BigInteger)(i_15)).longValue())])))));
            i_15 = new java.math.BigInteger(String.valueOf(i_15.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_5));
    }

    static double[] vec_map_sig(double[] v) {
        double[] res_6 = ((double[])(new double[]{}));
        java.math.BigInteger i_17 = java.math.BigInteger.valueOf(0);
        while (i_17.compareTo(new java.math.BigInteger(String.valueOf(v.length))) < 0) {
            res_6 = ((double[])(appendDouble(res_6, (double)(sigmoid((double)(v[_idx((v).length, ((java.math.BigInteger)(i_17)).longValue())]))))));
            i_17 = new java.math.BigInteger(String.valueOf(i_17.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_6));
    }

    static CNN new_cnn() {
        double[][] k1 = ((double[][])(new double[][]{((double[])(new double[]{(double)(1.0), (double)(0.0)})), ((double[])(new double[]{(double)(0.0), (double)(1.0)}))}));
        double[][] k2_1 = ((double[][])(new double[][]{((double[])(new double[]{(double)(0.0), (double)(1.0)})), ((double[])(new double[]{(double)(1.0), (double)(0.0)}))}));
        double[][][] conv_kernels_1 = ((double[][][])(new double[][][]{((double[][])(k1)), ((double[][])(k2_1))}));
        double[] conv_bias_1 = ((double[])(new double[]{(double)(0.0), (double)(0.0)}));
        java.math.BigInteger conv_step_1 = java.math.BigInteger.valueOf(2);
        java.math.BigInteger pool_size_1 = java.math.BigInteger.valueOf(2);
        java.math.BigInteger input_size_1 = java.math.BigInteger.valueOf(2);
        java.math.BigInteger hidden_size_1 = java.math.BigInteger.valueOf(2);
        java.math.BigInteger output_size_1 = java.math.BigInteger.valueOf(2);
        double[][] w_hidden_1 = ((double[][])(new double[][]{}));
        java.math.BigInteger i_19 = java.math.BigInteger.valueOf(0);
        while (i_19.compareTo(input_size_1) < 0) {
            double[] row_5 = ((double[])(new double[]{}));
            java.math.BigInteger j_11 = java.math.BigInteger.valueOf(0);
            while (j_11.compareTo(hidden_size_1) < 0) {
                row_5 = ((double[])(appendDouble(row_5, (double)((double)(random()) - (double)(0.5)))));
                j_11 = new java.math.BigInteger(String.valueOf(j_11.add(java.math.BigInteger.valueOf(1))));
            }
            w_hidden_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(w_hidden_1), java.util.stream.Stream.of(new double[][]{((double[])(row_5))})).toArray(double[][]::new)));
            i_19 = new java.math.BigInteger(String.valueOf(i_19.add(java.math.BigInteger.valueOf(1))));
        }
        double[][] w_out_1 = ((double[][])(new double[][]{}));
        i_19 = java.math.BigInteger.valueOf(0);
        while (i_19.compareTo(hidden_size_1) < 0) {
            double[] row_7 = ((double[])(new double[]{}));
            java.math.BigInteger j_13 = java.math.BigInteger.valueOf(0);
            while (j_13.compareTo(output_size_1) < 0) {
                row_7 = ((double[])(appendDouble(row_7, (double)((double)(random()) - (double)(0.5)))));
                j_13 = new java.math.BigInteger(String.valueOf(j_13.add(java.math.BigInteger.valueOf(1))));
            }
            w_out_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(w_out_1), java.util.stream.Stream.of(new double[][]{((double[])(row_7))})).toArray(double[][]::new)));
            i_19 = new java.math.BigInteger(String.valueOf(i_19.add(java.math.BigInteger.valueOf(1))));
        }
        double[] b_hidden_1 = ((double[])(new double[]{(double)(0.0), (double)(0.0)}));
        double[] b_out_1 = ((double[])(new double[]{(double)(0.0), (double)(0.0)}));
        return new CNN(((double[][][])(conv_kernels_1)), ((double[])(conv_bias_1)), new java.math.BigInteger(String.valueOf(conv_step_1)), new java.math.BigInteger(String.valueOf(pool_size_1)), ((double[][])(w_hidden_1)), ((double[][])(w_out_1)), ((double[])(b_hidden_1)), ((double[])(b_out_1)), (double)(0.2), (double)(0.2));
    }

    static double[] forward(CNN cnn, double[][] data) {
        double[][][] maps = ((double[][][])(new double[][][]{}));
        java.math.BigInteger i_21 = java.math.BigInteger.valueOf(0);
        while (i_21.compareTo(new java.math.BigInteger(String.valueOf(cnn.conv_kernels.length))) < 0) {
            double[][] conv_map_1 = ((double[][])(convolve(((double[][])(data)), ((double[][])(cnn.conv_kernels[_idx((cnn.conv_kernels).length, ((java.math.BigInteger)(i_21)).longValue())])), new java.math.BigInteger(String.valueOf(cnn.conv_step)), (double)(cnn.conv_bias[_idx((cnn.conv_bias).length, ((java.math.BigInteger)(i_21)).longValue())]))));
            double[][] pooled_1 = ((double[][])(average_pool(((double[][])(conv_map_1)), new java.math.BigInteger(String.valueOf(cnn.pool_size)))));
            maps = ((double[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(maps), java.util.stream.Stream.of(new double[][][]{((double[][])(pooled_1))})).toArray(double[][][]::new)));
            i_21 = new java.math.BigInteger(String.valueOf(i_21.add(java.math.BigInteger.valueOf(1))));
        }
        double[] flat_1 = ((double[])(flatten(((double[][][])(maps)))));
        double[] hidden_net_1 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(flat_1)), ((double[][])(cnn.w_hidden))))), ((double[])(cnn.b_hidden)))));
        double[] hidden_out_1 = ((double[])(vec_map_sig(((double[])(hidden_net_1)))));
        double[] out_net_1 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(hidden_out_1)), ((double[][])(cnn.w_out))))), ((double[])(cnn.b_out)))));
        double[] out_5 = ((double[])(vec_map_sig(((double[])(out_net_1)))));
        return ((double[])(out_5));
    }

    static CNN train(CNN cnn, TrainSample[] samples, java.math.BigInteger epochs) {
        double[][] w_out_2 = ((double[][])(cnn.w_out));
        double[] b_out_3 = ((double[])(cnn.b_out));
        double[][] w_hidden_3 = ((double[][])(cnn.w_hidden));
        double[] b_hidden_3 = ((double[])(cnn.b_hidden));
        java.math.BigInteger e_1 = java.math.BigInteger.valueOf(0);
        while (e_1.compareTo(epochs) < 0) {
            java.math.BigInteger s_1 = java.math.BigInteger.valueOf(0);
            while (s_1.compareTo(new java.math.BigInteger(String.valueOf(samples.length))) < 0) {
                double[][] data_1 = ((double[][])(samples[_idx((samples).length, ((java.math.BigInteger)(s_1)).longValue())].image));
                double[] target_1 = ((double[])(samples[_idx((samples).length, ((java.math.BigInteger)(s_1)).longValue())].target));
                double[][][] maps_2 = ((double[][][])(new double[][][]{}));
                java.math.BigInteger i_23 = java.math.BigInteger.valueOf(0);
                while (i_23.compareTo(new java.math.BigInteger(String.valueOf(cnn.conv_kernels.length))) < 0) {
                    double[][] conv_map_3 = ((double[][])(convolve(((double[][])(data_1)), ((double[][])(cnn.conv_kernels[_idx((cnn.conv_kernels).length, ((java.math.BigInteger)(i_23)).longValue())])), new java.math.BigInteger(String.valueOf(cnn.conv_step)), (double)(cnn.conv_bias[_idx((cnn.conv_bias).length, ((java.math.BigInteger)(i_23)).longValue())]))));
                    double[][] pooled_3 = ((double[][])(average_pool(((double[][])(conv_map_3)), new java.math.BigInteger(String.valueOf(cnn.pool_size)))));
                    maps_2 = ((double[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(maps_2), java.util.stream.Stream.of(new double[][][]{((double[][])(pooled_3))})).toArray(double[][][]::new)));
                    i_23 = new java.math.BigInteger(String.valueOf(i_23.add(java.math.BigInteger.valueOf(1))));
                }
                double[] flat_3 = ((double[])(flatten(((double[][][])(maps_2)))));
                double[] hidden_net_3 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(flat_3)), ((double[][])(w_hidden_3))))), ((double[])(b_hidden_3)))));
                double[] hidden_out_3 = ((double[])(vec_map_sig(((double[])(hidden_net_3)))));
                double[] out_net_3 = ((double[])(vec_add(((double[])(vec_mul_mat(((double[])(hidden_out_3)), ((double[][])(w_out_2))))), ((double[])(b_out_3)))));
                double[] out_7 = ((double[])(vec_map_sig(((double[])(out_net_3)))));
                double[] error_out_1 = ((double[])(vec_sub(((double[])(target_1)), ((double[])(out_7)))));
                double[] pd_out_1 = ((double[])(vec_mul(((double[])(error_out_1)), ((double[])(vec_mul(((double[])(out_7)), ((double[])(vec_sub(((double[])(new double[]{(double)(1.0), (double)(1.0)})), ((double[])(out_7)))))))))));
                double[] error_hidden_1 = ((double[])(matT_vec_mul(((double[][])(w_out_2)), ((double[])(pd_out_1)))));
                double[] pd_hidden_1 = ((double[])(vec_mul(((double[])(error_hidden_1)), ((double[])(vec_mul(((double[])(hidden_out_3)), ((double[])(vec_sub(((double[])(new double[]{(double)(1.0), (double)(1.0)})), ((double[])(hidden_out_3)))))))))));
                java.math.BigInteger j_15 = java.math.BigInteger.valueOf(0);
                while (j_15.compareTo(new java.math.BigInteger(String.valueOf(w_out_2.length))) < 0) {
                    java.math.BigInteger k_3 = java.math.BigInteger.valueOf(0);
                    while (k_3.compareTo(new java.math.BigInteger(String.valueOf(w_out_2[_idx((w_out_2).length, ((java.math.BigInteger)(j_15)).longValue())].length))) < 0) {
w_out_2[_idx((w_out_2).length, ((java.math.BigInteger)(j_15)).longValue())][(int)(((java.math.BigInteger)(k_3)).longValue())] = (double)((double)(w_out_2[_idx((w_out_2).length, ((java.math.BigInteger)(j_15)).longValue())][_idx((w_out_2[_idx((w_out_2).length, ((java.math.BigInteger)(j_15)).longValue())]).length, ((java.math.BigInteger)(k_3)).longValue())]) + (double)((double)((double)(cnn.rate_weight) * (double)(hidden_out_3[_idx((hidden_out_3).length, ((java.math.BigInteger)(j_15)).longValue())])) * (double)(pd_out_1[_idx((pd_out_1).length, ((java.math.BigInteger)(k_3)).longValue())])));
                        k_3 = new java.math.BigInteger(String.valueOf(k_3.add(java.math.BigInteger.valueOf(1))));
                    }
                    j_15 = new java.math.BigInteger(String.valueOf(j_15.add(java.math.BigInteger.valueOf(1))));
                }
                j_15 = java.math.BigInteger.valueOf(0);
                while (j_15.compareTo(new java.math.BigInteger(String.valueOf(b_out_3.length))) < 0) {
b_out_3[(int)(((java.math.BigInteger)(j_15)).longValue())] = (double)((double)(b_out_3[_idx((b_out_3).length, ((java.math.BigInteger)(j_15)).longValue())]) - (double)((double)(cnn.rate_bias) * (double)(pd_out_1[_idx((pd_out_1).length, ((java.math.BigInteger)(j_15)).longValue())])));
                    j_15 = new java.math.BigInteger(String.valueOf(j_15.add(java.math.BigInteger.valueOf(1))));
                }
                java.math.BigInteger i_h_1 = java.math.BigInteger.valueOf(0);
                while (i_h_1.compareTo(new java.math.BigInteger(String.valueOf(w_hidden_3.length))) < 0) {
                    java.math.BigInteger j_h_1 = java.math.BigInteger.valueOf(0);
                    while (j_h_1.compareTo(new java.math.BigInteger(String.valueOf(w_hidden_3[_idx((w_hidden_3).length, ((java.math.BigInteger)(i_h_1)).longValue())].length))) < 0) {
w_hidden_3[_idx((w_hidden_3).length, ((java.math.BigInteger)(i_h_1)).longValue())][(int)(((java.math.BigInteger)(j_h_1)).longValue())] = (double)((double)(w_hidden_3[_idx((w_hidden_3).length, ((java.math.BigInteger)(i_h_1)).longValue())][_idx((w_hidden_3[_idx((w_hidden_3).length, ((java.math.BigInteger)(i_h_1)).longValue())]).length, ((java.math.BigInteger)(j_h_1)).longValue())]) + (double)((double)((double)(cnn.rate_weight) * (double)(flat_3[_idx((flat_3).length, ((java.math.BigInteger)(i_h_1)).longValue())])) * (double)(pd_hidden_1[_idx((pd_hidden_1).length, ((java.math.BigInteger)(j_h_1)).longValue())])));
                        j_h_1 = new java.math.BigInteger(String.valueOf(j_h_1.add(java.math.BigInteger.valueOf(1))));
                    }
                    i_h_1 = new java.math.BigInteger(String.valueOf(i_h_1.add(java.math.BigInteger.valueOf(1))));
                }
                j_15 = java.math.BigInteger.valueOf(0);
                while (j_15.compareTo(new java.math.BigInteger(String.valueOf(b_hidden_3.length))) < 0) {
b_hidden_3[(int)(((java.math.BigInteger)(j_15)).longValue())] = (double)((double)(b_hidden_3[_idx((b_hidden_3).length, ((java.math.BigInteger)(j_15)).longValue())]) - (double)((double)(cnn.rate_bias) * (double)(pd_hidden_1[_idx((pd_hidden_1).length, ((java.math.BigInteger)(j_15)).longValue())])));
                    j_15 = new java.math.BigInteger(String.valueOf(j_15.add(java.math.BigInteger.valueOf(1))));
                }
                s_1 = new java.math.BigInteger(String.valueOf(s_1.add(java.math.BigInteger.valueOf(1))));
            }
            e_1 = new java.math.BigInteger(String.valueOf(e_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new CNN(((double[][][])(cnn.conv_kernels)), ((double[])(cnn.conv_bias)), new java.math.BigInteger(String.valueOf(cnn.conv_step)), new java.math.BigInteger(String.valueOf(cnn.pool_size)), ((double[][])(w_hidden_3)), ((double[][])(w_out_2)), ((double[])(b_hidden_3)), ((double[])(b_out_3)), (double)(cnn.rate_weight), (double)(cnn.rate_bias));
    }

    static void main() {
        CNN cnn = new_cnn();
        double[][] image_1 = ((double[][])(new double[][]{((double[])(new double[]{(double)(1.0), (double)(0.0), (double)(1.0), (double)(0.0)})), ((double[])(new double[]{(double)(0.0), (double)(1.0), (double)(0.0), (double)(1.0)})), ((double[])(new double[]{(double)(1.0), (double)(0.0), (double)(1.0), (double)(0.0)})), ((double[])(new double[]{(double)(0.0), (double)(1.0), (double)(0.0), (double)(1.0)}))}));
        TrainSample sample_1 = new TrainSample(((double[][])(image_1)), ((double[])(new double[]{(double)(1.0), (double)(0.0)})));
        System.out.println("Before training:" + " " + (String)(java.util.Arrays.toString(forward(cnn, ((double[][])(image_1))))));
        CNN trained_1 = train(cnn, ((TrainSample[])(new TrainSample[]{sample_1})), java.math.BigInteger.valueOf(50));
        System.out.println("After training:" + " " + (String)(java.util.Arrays.toString(forward(trained_1, ((double[][])(image_1))))));
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
