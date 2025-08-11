public class Main {
    static double[][] samples;
    static double[] labels;
    static double[][] model;

    static double dot(double[] a, double[] b) {
        double sum = 0.0;
        long i_1 = 0;
        while (i_1 < a.length) {
            sum = sum + a[(int)(i_1)] * b[(int)(i_1)];
            i_1 = i_1 + 1;
        }
        return sum;
    }

    static double maxf(double a, double b) {
        if (a > b) {
            return a;
        }
        return b;
    }

    static double minf(double a, double b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static double absf(double x) {
        if (x >= 0.0) {
            return x;
        }
        return 0.0 - x;
    }

    static double predict_raw(double[][] samples, double[] labels, double[] alphas, double b, double[] x) {
        double res = 0.0;
        long i_3 = 0;
        while (i_3 < samples.length) {
            res = res + alphas[(int)(i_3)] * labels[(int)(i_3)] * dot(((double[])(samples[(int)(i_3)])), ((double[])(x)));
            i_3 = i_3 + 1;
        }
        return res + b;
    }

    static double[][] smo_train(double[][] samples, double[] labels, double c, double tol, long max_passes) {
        long m = samples.length;
        double[] alphas_1 = ((double[])(new double[]{}));
        long i_5 = 0;
        while (i_5 < m) {
            alphas_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(alphas_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_5 = i_5 + 1;
        }
        double b_1 = 0.0;
        long passes_1 = 0;
        while (passes_1 < max_passes) {
            long num_changed_1 = 0;
            long i1_1 = 0;
            while (i1_1 < m) {
                double Ei_1 = predict_raw(((double[][])(samples)), ((double[])(labels)), ((double[])(alphas_1)), b_1, ((double[])(samples[(int)(i1_1)]))) - labels[(int)(i1_1)];
                if ((labels[(int)(i1_1)] * Ei_1 < 0.0 - tol && alphas_1[(int)(i1_1)] < c) || (labels[(int)(i1_1)] * Ei_1 > tol && alphas_1[(int)(i1_1)] > 0.0)) {
                    long i2_1 = Math.floorMod((i1_1 + 1), m);
                    double Ej_1 = predict_raw(((double[][])(samples)), ((double[])(labels)), ((double[])(alphas_1)), b_1, ((double[])(samples[(int)(i2_1)]))) - labels[(int)(i2_1)];
                    double alpha1_old_1 = alphas_1[(int)(i1_1)];
                    double alpha2_old_1 = alphas_1[(int)(i2_1)];
                    double L_1 = 0.0;
                    double H_1 = 0.0;
                    if (labels[(int)(i1_1)] != labels[(int)(i2_1)]) {
                        L_1 = maxf(0.0, alpha2_old_1 - alpha1_old_1);
                        H_1 = minf(c, c + alpha2_old_1 - alpha1_old_1);
                    } else {
                        L_1 = maxf(0.0, alpha2_old_1 + alpha1_old_1 - c);
                        H_1 = minf(c, alpha2_old_1 + alpha1_old_1);
                    }
                    if (L_1 == H_1) {
                        i1_1 = i1_1 + 1;
                        continue;
                    }
                    double eta_1 = 2.0 * dot(((double[])(samples[(int)(i1_1)])), ((double[])(samples[(int)(i2_1)]))) - dot(((double[])(samples[(int)(i1_1)])), ((double[])(samples[(int)(i1_1)]))) - dot(((double[])(samples[(int)(i2_1)])), ((double[])(samples[(int)(i2_1)])));
                    if (eta_1 >= 0.0) {
                        i1_1 = i1_1 + 1;
                        continue;
                    }
alphas_1[(int)(i2_1)] = alpha2_old_1 - labels[(int)(i2_1)] * (Ei_1 - Ej_1) / eta_1;
                    if (alphas_1[(int)(i2_1)] > H_1) {
alphas_1[(int)(i2_1)] = H_1;
                    }
                    if (alphas_1[(int)(i2_1)] < L_1) {
alphas_1[(int)(i2_1)] = L_1;
                    }
                    if (absf(alphas_1[(int)(i2_1)] - alpha2_old_1) < 1e-05) {
                        i1_1 = i1_1 + 1;
                        continue;
                    }
alphas_1[(int)(i1_1)] = alpha1_old_1 + labels[(int)(i1_1)] * labels[(int)(i2_1)] * (alpha2_old_1 - alphas_1[(int)(i2_1)]);
                    double b1_1 = b_1 - Ei_1 - labels[(int)(i1_1)] * (alphas_1[(int)(i1_1)] - alpha1_old_1) * dot(((double[])(samples[(int)(i1_1)])), ((double[])(samples[(int)(i1_1)]))) - labels[(int)(i2_1)] * (alphas_1[(int)(i2_1)] - alpha2_old_1) * dot(((double[])(samples[(int)(i1_1)])), ((double[])(samples[(int)(i2_1)])));
                    double b2_1 = b_1 - Ej_1 - labels[(int)(i1_1)] * (alphas_1[(int)(i1_1)] - alpha1_old_1) * dot(((double[])(samples[(int)(i1_1)])), ((double[])(samples[(int)(i2_1)]))) - labels[(int)(i2_1)] * (alphas_1[(int)(i2_1)] - alpha2_old_1) * dot(((double[])(samples[(int)(i2_1)])), ((double[])(samples[(int)(i2_1)])));
                    if (alphas_1[(int)(i1_1)] > 0.0 && alphas_1[(int)(i1_1)] < c) {
                        b_1 = b1_1;
                    } else                     if (alphas_1[(int)(i2_1)] > 0.0 && alphas_1[(int)(i2_1)] < c) {
                        b_1 = b2_1;
                    } else {
                        b_1 = (b1_1 + b2_1) / 2.0;
                    }
                    num_changed_1 = num_changed_1 + 1;
                }
                i1_1 = i1_1 + 1;
            }
            if (num_changed_1 == 0) {
                passes_1 = passes_1 + 1;
            } else {
                passes_1 = 0;
            }
        }
        return new double[][]{alphas_1, new double[]{b_1}};
    }

    static double predict(double[][] samples, double[] labels, double[][] model, double[] x) {
        double[] alphas_2 = ((double[])(model[(int)(0)]));
        double b_3 = model[(int)(1)][(int)(0)];
        double val_1 = predict_raw(((double[][])(samples)), ((double[])(labels)), ((double[])(alphas_2)), b_3, ((double[])(x)));
        if (val_1 >= 0.0) {
            return 1.0;
        }
        return -1.0;
    }
    public static void main(String[] args) {
        samples = ((double[][])(new double[][]{new double[]{2.0, 2.0}, new double[]{1.5, 1.5}, new double[]{0.0, 0.0}, new double[]{0.5, 0.0}}));
        labels = ((double[])(new double[]{1.0, 1.0, -1.0, -1.0}));
        model = ((double[][])(smo_train(((double[][])(samples)), ((double[])(labels)), 1.0, 0.001, 10)));
        System.out.println(predict(((double[][])(samples)), ((double[])(labels)), ((double[][])(model)), ((double[])(new double[]{1.5, 1.0}))));
        System.out.println(predict(((double[][])(samples)), ((double[])(labels)), ((double[][])(model)), ((double[])(new double[]{0.2, 0.1}))));
    }
}
