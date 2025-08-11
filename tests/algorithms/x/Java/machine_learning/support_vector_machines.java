public class Main {
    static class SVC {
        double[] weights;
        double bias;
        double lr;
        double lambda;
        long epochs;
        SVC(double[] weights, double bias, double lr, double lambda, long epochs) {
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
    static long[] ys;
    static SVC base;
    static SVC model;

    static double dot(double[] a, double[] b) {
        double s = 0.0;
        long i_1 = 0;
        while (i_1 < a.length) {
            s = s + a[(int)(i_1)] * b[(int)(i_1)];
            i_1 = i_1 + 1;
        }
        return s;
    }

    static SVC new_svc(double lr, double lambda, long epochs) {
        return new SVC(new double[]{}, 0.0, lr, lambda, epochs);
    }

    static SVC fit(SVC model, double[][] xs, long[] ys) {
        long n_features = xs[(int)(0)].length;
        double[] w_1 = ((double[])(new double[]{}));
        long i_3 = 0;
        while (i_3 < n_features) {
            w_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(w_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_3 = i_3 + 1;
        }
        double b_1 = 0.0;
        long epoch_1 = 0;
        while (epoch_1 < model.epochs) {
            long j_1 = 0;
            while (j_1 < xs.length) {
                double[] x_1 = ((double[])(xs[(int)(j_1)]));
                double y_1 = ((double)(ys[(int)(j_1)]));
                double prod_1 = dot(((double[])(w_1)), ((double[])(x_1))) + b_1;
                if (y_1 * prod_1 < 1.0) {
                    long k_2 = 0;
                    while (k_2 < w_1.length) {
w_1[(int)(k_2)] = w_1[(int)(k_2)] + model.lr * (y_1 * x_1[(int)(k_2)] - 2.0 * model.lambda * w_1[(int)(k_2)]);
                        k_2 = k_2 + 1;
                    }
                    b_1 = b_1 + model.lr * y_1;
                } else {
                    long k_3 = 0;
                    while (k_3 < w_1.length) {
w_1[(int)(k_3)] = w_1[(int)(k_3)] - model.lr * (2.0 * model.lambda * w_1[(int)(k_3)]);
                        k_3 = k_3 + 1;
                    }
                }
                j_1 = j_1 + 1;
            }
            epoch_1 = epoch_1 + 1;
        }
        return new SVC(w_1, b_1, model.lr, model.lambda, model.epochs);
    }

    static long predict(SVC model, double[] x) {
        double s_1 = dot(((double[])(model.weights)), ((double[])(x))) + model.bias;
        if (s_1 >= 0.0) {
            return 1;
        } else {
            return -1;
        }
    }
    public static void main(String[] args) {
        xs = ((double[][])(new double[][]{new double[]{0.0, 1.0}, new double[]{0.0, 2.0}, new double[]{1.0, 1.0}, new double[]{1.0, 2.0}}));
        ys = ((long[])(new long[]{1, 1, -1, -1}));
        base = new_svc(0.01, 0.01, 1000);
        model = fit(base, ((double[][])(xs)), ((long[])(ys)));
        System.out.println(predict(model, ((double[])(new double[]{0.0, 1.0}))));
        System.out.println(predict(model, ((double[])(new double[]{1.0, 1.0}))));
        System.out.println(predict(model, ((double[])(new double[]{2.0, 2.0}))));
    }
}
