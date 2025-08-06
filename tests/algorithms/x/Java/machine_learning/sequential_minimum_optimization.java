public class Main {
    static double[][] samples;
    static double[] labels;
    static double[][] model;

    static double dot(double[] a, double[] b) {
        double sum = 0.0;
        int i = 0;
        while (i < a.length) {
            sum = sum + a[i] * b[i];
            i = i + 1;
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
        int i_1 = 0;
        while (i_1 < samples.length) {
            res = res + alphas[i_1] * labels[i_1] * dot(((double[])(samples[i_1])), ((double[])(x)));
            i_1 = i_1 + 1;
        }
        return res + b;
    }

    static double[][] smo_train(double[][] samples, double[] labels, double c, double tol, int max_passes) {
        int m = samples.length;
        double[] alphas = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < m) {
            alphas = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(alphas), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_2 = i_2 + 1;
        }
        double b = 0.0;
        int passes = 0;
        while (passes < max_passes) {
            int num_changed = 0;
            int i1 = 0;
            while (i1 < m) {
                double Ei = predict_raw(((double[][])(samples)), ((double[])(labels)), ((double[])(alphas)), b, ((double[])(samples[i1]))) - labels[i1];
                if ((labels[i1] * Ei < 0.0 - tol && alphas[i1] < c) || (labels[i1] * Ei > tol && alphas[i1] > 0.0)) {
                    int i2 = Math.floorMod((i1 + 1), m);
                    double Ej = predict_raw(((double[][])(samples)), ((double[])(labels)), ((double[])(alphas)), b, ((double[])(samples[i2]))) - labels[i2];
                    double alpha1_old = alphas[i1];
                    double alpha2_old = alphas[i2];
                    double L = 0.0;
                    double H = 0.0;
                    if (labels[i1] != labels[i2]) {
                        L = maxf(0.0, alpha2_old - alpha1_old);
                        H = minf(c, c + alpha2_old - alpha1_old);
                    } else {
                        L = maxf(0.0, alpha2_old + alpha1_old - c);
                        H = minf(c, alpha2_old + alpha1_old);
                    }
                    if (L == H) {
                        i1 = i1 + 1;
                        continue;
                    }
                    double eta = 2.0 * dot(((double[])(samples[i1])), ((double[])(samples[i2]))) - dot(((double[])(samples[i1])), ((double[])(samples[i1]))) - dot(((double[])(samples[i2])), ((double[])(samples[i2])));
                    if (eta >= 0.0) {
                        i1 = i1 + 1;
                        continue;
                    }
alphas[i2] = alpha2_old - labels[i2] * (Ei - Ej) / eta;
                    if (alphas[i2] > H) {
alphas[i2] = H;
                    }
                    if (alphas[i2] < L) {
alphas[i2] = L;
                    }
                    if (absf(alphas[i2] - alpha2_old) < 1e-05) {
                        i1 = i1 + 1;
                        continue;
                    }
alphas[i1] = alpha1_old + labels[i1] * labels[i2] * (alpha2_old - alphas[i2]);
                    double b1 = b - Ei - labels[i1] * (alphas[i1] - alpha1_old) * dot(((double[])(samples[i1])), ((double[])(samples[i1]))) - labels[i2] * (alphas[i2] - alpha2_old) * dot(((double[])(samples[i1])), ((double[])(samples[i2])));
                    double b2 = b - Ej - labels[i1] * (alphas[i1] - alpha1_old) * dot(((double[])(samples[i1])), ((double[])(samples[i2]))) - labels[i2] * (alphas[i2] - alpha2_old) * dot(((double[])(samples[i2])), ((double[])(samples[i2])));
                    if (alphas[i1] > 0.0 && alphas[i1] < c) {
                        b = b1;
                    } else                     if (alphas[i2] > 0.0 && alphas[i2] < c) {
                        b = b2;
                    } else {
                        b = (b1 + b2) / 2.0;
                    }
                    num_changed = num_changed + 1;
                }
                i1 = i1 + 1;
            }
            if (num_changed == 0) {
                passes = passes + 1;
            } else {
                passes = 0;
            }
        }
        return new double[][]{alphas, new double[]{b}};
    }

    static double predict(double[][] samples, double[] labels, double[][] model, double[] x) {
        double[] alphas_1 = ((double[])(model[0]));
        double b_1 = model[1][0];
        double val = predict_raw(((double[][])(samples)), ((double[])(labels)), ((double[])(alphas_1)), b_1, ((double[])(x)));
        if (val >= 0.0) {
            return 1.0;
        }
        return -1.0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            samples = ((double[][])(new double[][]{new double[]{2.0, 2.0}, new double[]{1.5, 1.5}, new double[]{0.0, 0.0}, new double[]{0.5, 0.0}}));
            labels = ((double[])(new double[]{1.0, 1.0, -1.0, -1.0}));
            model = ((double[][])(smo_train(((double[][])(samples)), ((double[])(labels)), 1.0, 0.001, 10)));
            System.out.println(predict(((double[][])(samples)), ((double[])(labels)), ((double[][])(model)), ((double[])(new double[]{1.5, 1.0}))));
            System.out.println(predict(((double[][])(samples)), ((double[])(labels)), ((double[][])(model)), ((double[])(new double[]{0.2, 0.1}))));
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
}
