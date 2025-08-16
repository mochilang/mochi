public class Main {
    static double[][] samples = ((double[][])(new double[][]{new double[]{2.0, 2.0}, new double[]{1.5, 1.5}, new double[]{0.0, 0.0}, new double[]{0.5, 0.0}}));
    static double[] labels;
    static double[][] model;

    static double dot(double[] a, double[] b) {
        double sum = (double)(0.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(a.length)) {
            sum = (double)((double)(sum) + (double)((double)(a[(int)((long)(i_1))]) * (double)(b[(int)((long)(i_1))])));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return sum;
    }

    static double maxf(double a, double b) {
        if ((double)(a) > (double)(b)) {
            return a;
        }
        return b;
    }

    static double minf(double a, double b) {
        if ((double)(a) < (double)(b)) {
            return a;
        }
        return b;
    }

    static double absf(double x) {
        if ((double)(x) >= (double)(0.0)) {
            return x;
        }
        return (double)(0.0) - (double)(x);
    }

    static double predict_raw(double[][] samples, double[] labels, double[] alphas, double b, double[] x) {
        double res = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(samples.length)) {
            res = (double)((double)(res) + (double)((double)((double)(alphas[(int)((long)(i_3))]) * (double)(labels[(int)((long)(i_3))])) * (double)(dot(((double[])(samples[(int)((long)(i_3))])), ((double[])(x))))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (double)(res) + (double)(b);
    }

    static double[][] smo_train(double[][] samples, double[] labels, double c, double tol, long max_passes) {
        long m = (long)(samples.length);
        double[] alphas_1 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(m)) {
            alphas_1 = ((double[])(appendDouble(alphas_1, (double)(0.0))));
            i_5 = (long)((long)(i_5) + 1L);
        }
        double b_1 = (double)(0.0);
        long passes_1 = 0L;
        while ((long)(passes_1) < (long)(max_passes)) {
            long num_changed_1 = 0L;
            long i1_1 = 0L;
            while ((long)(i1_1) < (long)(m)) {
                double Ei_1 = (double)((double)(predict_raw(((double[][])(samples)), ((double[])(labels)), ((double[])(alphas_1)), (double)(b_1), ((double[])(samples[(int)((long)(i1_1))])))) - (double)(labels[(int)((long)(i1_1))]));
                if (((double)((double)(labels[(int)((long)(i1_1))]) * (double)(Ei_1)) < (double)((double)(0.0) - (double)(tol)) && (double)(alphas_1[(int)((long)(i1_1))]) < (double)(c)) || ((double)((double)(labels[(int)((long)(i1_1))]) * (double)(Ei_1)) > (double)(tol) && (double)(alphas_1[(int)((long)(i1_1))]) > (double)(0.0))) {
                    long i2_1 = Math.floorMod(((long)(i1_1) + 1L), m);
                    double Ej_1 = (double)((double)(predict_raw(((double[][])(samples)), ((double[])(labels)), ((double[])(alphas_1)), (double)(b_1), ((double[])(samples[(int)((long)(i2_1))])))) - (double)(labels[(int)((long)(i2_1))]));
                    double alpha1_old_1 = (double)(alphas_1[(int)((long)(i1_1))]);
                    double alpha2_old_1 = (double)(alphas_1[(int)((long)(i2_1))]);
                    double L_1 = (double)(0.0);
                    double H_1 = (double)(0.0);
                    if ((double)(labels[(int)((long)(i1_1))]) != (double)(labels[(int)((long)(i2_1))])) {
                        L_1 = (double)(maxf((double)(0.0), (double)((double)(alpha2_old_1) - (double)(alpha1_old_1))));
                        H_1 = (double)(minf((double)(c), (double)((double)((double)(c) + (double)(alpha2_old_1)) - (double)(alpha1_old_1))));
                    } else {
                        L_1 = (double)(maxf((double)(0.0), (double)((double)((double)(alpha2_old_1) + (double)(alpha1_old_1)) - (double)(c))));
                        H_1 = (double)(minf((double)(c), (double)((double)(alpha2_old_1) + (double)(alpha1_old_1))));
                    }
                    if ((double)(L_1) == (double)(H_1)) {
                        i1_1 = (long)((long)(i1_1) + 1L);
                        continue;
                    }
                    double eta_1 = (double)((double)((double)((double)(2.0) * (double)(dot(((double[])(samples[(int)((long)(i1_1))])), ((double[])(samples[(int)((long)(i2_1))]))))) - (double)(dot(((double[])(samples[(int)((long)(i1_1))])), ((double[])(samples[(int)((long)(i1_1))]))))) - (double)(dot(((double[])(samples[(int)((long)(i2_1))])), ((double[])(samples[(int)((long)(i2_1))])))));
                    if ((double)(eta_1) >= (double)(0.0)) {
                        i1_1 = (long)((long)(i1_1) + 1L);
                        continue;
                    }
alphas_1[(int)((long)(i2_1))] = (double)((double)(alpha2_old_1) - (double)((double)((double)(labels[(int)((long)(i2_1))]) * (double)(((double)(Ei_1) - (double)(Ej_1)))) / (double)(eta_1)));
                    if ((double)(alphas_1[(int)((long)(i2_1))]) > (double)(H_1)) {
alphas_1[(int)((long)(i2_1))] = (double)(H_1);
                    }
                    if ((double)(alphas_1[(int)((long)(i2_1))]) < (double)(L_1)) {
alphas_1[(int)((long)(i2_1))] = (double)(L_1);
                    }
                    if ((double)(absf((double)((double)(alphas_1[(int)((long)(i2_1))]) - (double)(alpha2_old_1)))) < (double)(1e-05)) {
                        i1_1 = (long)((long)(i1_1) + 1L);
                        continue;
                    }
alphas_1[(int)((long)(i1_1))] = (double)((double)(alpha1_old_1) + (double)((double)((double)(labels[(int)((long)(i1_1))]) * (double)(labels[(int)((long)(i2_1))])) * (double)(((double)(alpha2_old_1) - (double)(alphas_1[(int)((long)(i2_1))])))));
                    double b1_1 = (double)((double)((double)((double)(b_1) - (double)(Ei_1)) - (double)((double)((double)(labels[(int)((long)(i1_1))]) * (double)(((double)(alphas_1[(int)((long)(i1_1))]) - (double)(alpha1_old_1)))) * (double)(dot(((double[])(samples[(int)((long)(i1_1))])), ((double[])(samples[(int)((long)(i1_1))])))))) - (double)((double)((double)(labels[(int)((long)(i2_1))]) * (double)(((double)(alphas_1[(int)((long)(i2_1))]) - (double)(alpha2_old_1)))) * (double)(dot(((double[])(samples[(int)((long)(i1_1))])), ((double[])(samples[(int)((long)(i2_1))]))))));
                    double b2_1 = (double)((double)((double)((double)(b_1) - (double)(Ej_1)) - (double)((double)((double)(labels[(int)((long)(i1_1))]) * (double)(((double)(alphas_1[(int)((long)(i1_1))]) - (double)(alpha1_old_1)))) * (double)(dot(((double[])(samples[(int)((long)(i1_1))])), ((double[])(samples[(int)((long)(i2_1))])))))) - (double)((double)((double)(labels[(int)((long)(i2_1))]) * (double)(((double)(alphas_1[(int)((long)(i2_1))]) - (double)(alpha2_old_1)))) * (double)(dot(((double[])(samples[(int)((long)(i2_1))])), ((double[])(samples[(int)((long)(i2_1))]))))));
                    if ((double)(alphas_1[(int)((long)(i1_1))]) > (double)(0.0) && (double)(alphas_1[(int)((long)(i1_1))]) < (double)(c)) {
                        b_1 = (double)(b1_1);
                    } else                     if ((double)(alphas_1[(int)((long)(i2_1))]) > (double)(0.0) && (double)(alphas_1[(int)((long)(i2_1))]) < (double)(c)) {
                        b_1 = (double)(b2_1);
                    } else {
                        b_1 = (double)((double)(((double)(b1_1) + (double)(b2_1))) / (double)(2.0));
                    }
                    num_changed_1 = (long)((long)(num_changed_1) + 1L);
                }
                i1_1 = (long)((long)(i1_1) + 1L);
            }
            if ((long)(num_changed_1) == 0L) {
                passes_1 = (long)((long)(passes_1) + 1L);
            } else {
                passes_1 = 0L;
            }
        }
        return new double[][]{alphas_1, new double[]{b_1}};
    }

    static double predict(double[][] samples, double[] labels, double[][] model, double[] x) {
        double[] alphas_2 = ((double[])(model[(int)(0L)]));
        double b_3 = (double)(model[(int)(1L)][(int)(0L)]);
        double val_1 = (double)(predict_raw(((double[][])(samples)), ((double[])(labels)), ((double[])(alphas_2)), (double)(b_3), ((double[])(x))));
        if ((double)(val_1) >= (double)(0.0)) {
            return 1.0;
        }
        return -1.0;
    }
    public static void main(String[] args) {
        labels = ((double[])(new double[]{1.0, 1.0, -1.0, -1.0}));
        model = ((double[][])(smo_train(((double[][])(samples)), ((double[])(labels)), (double)(1.0), (double)(0.001), 10L)));
        System.out.println(predict(((double[][])(samples)), ((double[])(labels)), ((double[][])(model)), ((double[])(new double[]{1.5, 1.0}))));
        System.out.println(predict(((double[][])(samples)), ((double[])(labels)), ((double[][])(model)), ((double[])(new double[]{0.2, 0.1}))));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
