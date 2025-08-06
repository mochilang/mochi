public class Main {
    static class SVC {
        double[] weights;
        double bias;
        double lr;
        double lambda;
        int epochs;
        SVC(double[] weights, double bias, double lr, double lambda, int epochs) {
            this.weights = weights;
            this.bias = bias;
            this.lr = lr;
            this.lambda = lambda;
            this.epochs = epochs;
        }
        SVC() {}
        @Override public String toString() {
            return String.format("{'weights': %s, 'bias': %s, 'lr': %s, 'lambda': %s, 'epochs': %s}", String.valueOf(weights), String.valueOf(bias), String.valueOf(lr), String.valueOf(lambda), String.valueOf(epochs));
        }
    }

    static double[][] xs;
    static int[] ys;
    static SVC base;
    static SVC model;

    static double dot(double[] a, double[] b) {
        double s = 0.0;
        int i = 0;
        while (i < a.length) {
            s = s + a[i] * b[i];
            i = i + 1;
        }
        return s;
    }

    static SVC new_svc(double lr, double lambda, int epochs) {
        return new SVC(new double[]{}, 0.0, lr, lambda, epochs);
    }

    static SVC fit(SVC model, double[][] xs, int[] ys) {
        int n_features = xs[0].length;
        double[] w = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < n_features) {
            w = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(w), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_1 = i_1 + 1;
        }
        double b = 0.0;
        int epoch = 0;
        while (epoch < model.epochs) {
            int j = 0;
            while (j < xs.length) {
                double[] x = ((double[])(xs[j]));
                double y = ((double)(ys[j]));
                double prod = dot(((double[])(w)), ((double[])(x))) + b;
                if (y * prod < 1.0) {
                    int k = 0;
                    while (k < w.length) {
w[k] = w[k] + model.lr * (y * x[k] - 2.0 * model.lambda * w[k]);
                        k = k + 1;
                    }
                    b = b + model.lr * y;
                } else {
                    int k_1 = 0;
                    while (k_1 < w.length) {
w[k_1] = w[k_1] - model.lr * (2.0 * model.lambda * w[k_1]);
                        k_1 = k_1 + 1;
                    }
                }
                j = j + 1;
            }
            epoch = epoch + 1;
        }
        return new SVC(w, b, model.lr, model.lambda, model.epochs);
    }

    static int predict(SVC model, double[] x) {
        double s_1 = dot(((double[])(model.weights)), ((double[])(x))) + model.bias;
        if (s_1 >= 0.0) {
            return 1;
        } else {
            return -1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            xs = ((double[][])(new double[][]{new double[]{0.0, 1.0}, new double[]{0.0, 2.0}, new double[]{1.0, 1.0}, new double[]{1.0, 2.0}}));
            ys = ((int[])(new int[]{1, 1, -1, -1}));
            base = new_svc(0.01, 0.01, 1000);
            model = fit(base, ((double[][])(xs)), ((int[])(ys)));
            System.out.println(predict(model, ((double[])(new double[]{0.0, 1.0}))));
            System.out.println(predict(model, ((double[])(new double[]{1.0, 1.0}))));
            System.out.println(predict(model, ((double[])(new double[]{2.0, 2.0}))));
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
