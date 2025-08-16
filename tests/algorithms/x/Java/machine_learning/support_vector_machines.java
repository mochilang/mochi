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

    static double[][] xs = ((double[][])(new double[][]{new double[]{0.0, 1.0}, new double[]{0.0, 2.0}, new double[]{1.0, 1.0}, new double[]{1.0, 2.0}}));
    static long[] ys;
    static SVC base;
    static SVC model;

    static double dot(double[] a, double[] b) {
        double s = (double)(0.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(a.length)) {
            s = (double)((double)(s) + (double)((double)(a[(int)((long)(i_1))]) * (double)(b[(int)((long)(i_1))])));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return s;
    }

    static SVC new_svc(double lr, double lambda, long epochs) {
        return new SVC(new double[]{}, 0.0, lr, lambda, epochs);
    }

    static SVC fit(SVC model, double[][] xs, long[] ys) {
        long n_features = (long)(xs[(int)(0L)].length);
        double[] w_1 = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n_features)) {
            w_1 = ((double[])(appendDouble(w_1, (double)(0.0))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        double b_1 = (double)(0.0);
        long epoch_1 = 0L;
        while ((long)(epoch_1) < (long)(model.epochs)) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)(xs.length)) {
                double[] x_1 = ((double[])(xs[(int)((long)(j_1))]));
                double y_1 = (double)(((double)(ys[(int)((long)(j_1))])));
                double prod_1 = (double)((double)(dot(((double[])(w_1)), ((double[])(x_1)))) + (double)(b_1));
                if ((double)((double)(y_1) * (double)(prod_1)) < (double)(1.0)) {
                    long k_2 = 0L;
                    while ((long)(k_2) < (long)(w_1.length)) {
w_1[(int)((long)(k_2))] = (double)((double)(w_1[(int)((long)(k_2))]) + (double)((double)(model.lr) * (double)(((double)((double)(y_1) * (double)(x_1[(int)((long)(k_2))])) - (double)((double)((double)(2.0) * (double)(model.lambda)) * (double)(w_1[(int)((long)(k_2))]))))));
                        k_2 = (long)((long)(k_2) + 1L);
                    }
                    b_1 = (double)((double)(b_1) + (double)((double)(model.lr) * (double)(y_1)));
                } else {
                    long k_3 = 0L;
                    while ((long)(k_3) < (long)(w_1.length)) {
w_1[(int)((long)(k_3))] = (double)((double)(w_1[(int)((long)(k_3))]) - (double)((double)(model.lr) * (double)(((double)((double)(2.0) * (double)(model.lambda)) * (double)(w_1[(int)((long)(k_3))])))));
                        k_3 = (long)((long)(k_3) + 1L);
                    }
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            epoch_1 = (long)((long)(epoch_1) + 1L);
        }
        return new SVC(w_1, b_1, model.lr, model.lambda, model.epochs);
    }

    static long predict(SVC model, double[] x) {
        double s_1 = (double)((double)(dot(((double[])(model.weights)), ((double[])(x)))) + (double)(model.bias));
        if ((double)(s_1) >= (double)(0.0)) {
            return 1;
        } else {
            return -1;
        }
    }
    public static void main(String[] args) {
        ys = ((long[])(new long[]{1, 1, -1, -1}));
        base = new_svc((double)(0.01), (double)(0.01), 1000L);
        model = fit(base, ((double[][])(xs)), ((long[])(ys)));
        System.out.println(predict(model, ((double[])(new double[]{0.0, 1.0}))));
        System.out.println(predict(model, ((double[])(new double[]{1.0, 1.0}))));
        System.out.println(predict(model, ((double[])(new double[]{2.0, 2.0}))));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
