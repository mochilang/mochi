public class Main {
    static class LSTMWeights {
        double w_i;
        double u_i;
        double b_i;
        double w_f;
        double u_f;
        double b_f;
        double w_o;
        double u_o;
        double b_o;
        double w_c;
        double u_c;
        double b_c;
        double w_y;
        double b_y;
        LSTMWeights(double w_i, double u_i, double b_i, double w_f, double u_f, double b_f, double w_o, double u_o, double b_o, double w_c, double u_c, double b_c, double w_y, double b_y) {
            this.w_i = w_i;
            this.u_i = u_i;
            this.b_i = b_i;
            this.w_f = w_f;
            this.u_f = u_f;
            this.b_f = b_f;
            this.w_o = w_o;
            this.u_o = u_o;
            this.b_o = b_o;
            this.w_c = w_c;
            this.u_c = u_c;
            this.b_c = b_c;
            this.w_y = w_y;
            this.b_y = b_y;
        }
        LSTMWeights() {}
        @Override public String toString() {
            return String.format("{'w_i': %s, 'u_i': %s, 'b_i': %s, 'w_f': %s, 'u_f': %s, 'b_f': %s, 'w_o': %s, 'u_o': %s, 'b_o': %s, 'w_c': %s, 'u_c': %s, 'b_c': %s, 'w_y': %s, 'b_y': %s}", String.valueOf(w_i), String.valueOf(u_i), String.valueOf(b_i), String.valueOf(w_f), String.valueOf(u_f), String.valueOf(b_f), String.valueOf(w_o), String.valueOf(u_o), String.valueOf(b_o), String.valueOf(w_c), String.valueOf(u_c), String.valueOf(b_c), String.valueOf(w_y), String.valueOf(b_y));
        }
    }

    static class LSTMState {
        double[] i;
        double[] f;
        double[] o;
        double[] g;
        double[] c;
        double[] h;
        LSTMState(double[] i, double[] f, double[] o, double[] g, double[] c, double[] h) {
            this.i = i;
            this.f = f;
            this.o = o;
            this.g = g;
            this.c = c;
            this.h = h;
        }
        LSTMState() {}
        @Override public String toString() {
            return String.format("{'i': %s, 'f': %s, 'o': %s, 'g': %s, 'c': %s, 'h': %s}", String.valueOf(i), String.valueOf(f), String.valueOf(o), String.valueOf(g), String.valueOf(c), String.valueOf(h));
        }
    }

    static class Samples {
        double[][] x;
        double[] y;
        Samples(double[][] x, double[] y) {
            this.x = x;
            this.y = y;
        }
        Samples() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static double[] data;
    static long look_back;
    static long epochs;
    static double lr;
    static LSTMWeights w_2;
    static double[] test_seq;
    static double pred;

    static double exp_approx(double x) {
        double sum = 1.0;
        double term_1 = 1.0;
        long n_1 = 1;
        while (n_1 < 20) {
            term_1 = term_1 * x / (((Number)(n_1)).doubleValue());
            sum = sum + term_1;
            n_1 = n_1 + 1;
        }
        return sum;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + exp_approx(-x));
    }

    static double tanh_approx(double x) {
        double e = exp_approx(2.0 * x);
        return (e - 1.0) / (e + 1.0);
    }

    static LSTMState forward(double[] seq, LSTMWeights w) {
        double[] i_arr = ((double[])(new double[]{}));
        double[] f_arr_1 = ((double[])(new double[]{}));
        double[] o_arr_1 = ((double[])(new double[]{}));
        double[] g_arr_1 = ((double[])(new double[]{}));
        double[] c_arr_1 = ((double[])(new double[]{0.0}));
        double[] h_arr_1 = ((double[])(new double[]{0.0}));
        long t_1 = 0;
        while (t_1 < seq.length) {
            double x_1 = seq[(int)(t_1)];
            double h_prev_1 = h_arr_1[(int)(t_1)];
            double c_prev_1 = c_arr_1[(int)(t_1)];
            double i_t_1 = sigmoid(w.w_i * x_1 + w.u_i * h_prev_1 + w.b_i);
            double f_t_1 = sigmoid(w.w_f * x_1 + w.u_f * h_prev_1 + w.b_f);
            double o_t_1 = sigmoid(w.w_o * x_1 + w.u_o * h_prev_1 + w.b_o);
            double g_t_1 = tanh_approx(w.w_c * x_1 + w.u_c * h_prev_1 + w.b_c);
            double c_t_1 = f_t_1 * c_prev_1 + i_t_1 * g_t_1;
            double h_t_1 = o_t_1 * tanh_approx(c_t_1);
            i_arr = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(i_arr), java.util.stream.DoubleStream.of(i_t_1)).toArray()));
            f_arr_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(f_arr_1), java.util.stream.DoubleStream.of(f_t_1)).toArray()));
            o_arr_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(o_arr_1), java.util.stream.DoubleStream.of(o_t_1)).toArray()));
            g_arr_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(g_arr_1), java.util.stream.DoubleStream.of(g_t_1)).toArray()));
            c_arr_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(c_arr_1), java.util.stream.DoubleStream.of(c_t_1)).toArray()));
            h_arr_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(h_arr_1), java.util.stream.DoubleStream.of(h_t_1)).toArray()));
            t_1 = t_1 + 1;
        }
        return new LSTMState(i_arr, f_arr_1, o_arr_1, g_arr_1, c_arr_1, h_arr_1);
    }

    static LSTMWeights backward(double[] seq, double target, LSTMWeights w, LSTMState s, double lr) {
        double dw_i = 0.0;
        double du_i_1 = 0.0;
        double db_i_1 = 0.0;
        double dw_f_1 = 0.0;
        double du_f_1 = 0.0;
        double db_f_1 = 0.0;
        double dw_o_1 = 0.0;
        double du_o_1 = 0.0;
        double db_o_1 = 0.0;
        double dw_c_1 = 0.0;
        double du_c_1 = 0.0;
        double db_c_1 = 0.0;
        double dw_y_1 = 0.0;
        double db_y_1 = 0.0;
        long T_1 = seq.length;
        double h_last_1 = s.h[(int)(T_1)];
        double y_1 = w.w_y * h_last_1 + w.b_y;
        double dy_1 = y_1 - target;
        dw_y_1 = dy_1 * h_last_1;
        db_y_1 = dy_1;
        double dh_next_1 = dy_1 * w.w_y;
        double dc_next_1 = 0.0;
        long t_3 = T_1 - 1;
        while (t_3 >= 0) {
            double i_t_3 = s.i[(int)(t_3)];
            double f_t_3 = s.f[(int)(t_3)];
            double o_t_3 = s.o[(int)(t_3)];
            double g_t_3 = s.g[(int)(t_3)];
            double c_t_3 = s.c[(int)(t_3 + 1)];
            double c_prev_3 = s.c[(int)(t_3)];
            double h_prev_3 = s.h[(int)(t_3)];
            double tanh_c_1 = tanh_approx(c_t_3);
            double do_t_1 = dh_next_1 * tanh_c_1;
            double da_o_1 = do_t_1 * o_t_3 * (1.0 - o_t_3);
            double dc_1 = dh_next_1 * o_t_3 * (1.0 - tanh_c_1 * tanh_c_1) + dc_next_1;
            double di_t_1 = dc_1 * g_t_3;
            double da_i_1 = di_t_1 * i_t_3 * (1.0 - i_t_3);
            double dg_t_1 = dc_1 * i_t_3;
            double da_g_1 = dg_t_1 * (1.0 - g_t_3 * g_t_3);
            double df_t_1 = dc_1 * c_prev_3;
            double da_f_1 = df_t_1 * f_t_3 * (1.0 - f_t_3);
            dw_i = dw_i + da_i_1 * seq[(int)(t_3)];
            du_i_1 = du_i_1 + da_i_1 * h_prev_3;
            db_i_1 = db_i_1 + da_i_1;
            dw_f_1 = dw_f_1 + da_f_1 * seq[(int)(t_3)];
            du_f_1 = du_f_1 + da_f_1 * h_prev_3;
            db_f_1 = db_f_1 + da_f_1;
            dw_o_1 = dw_o_1 + da_o_1 * seq[(int)(t_3)];
            du_o_1 = du_o_1 + da_o_1 * h_prev_3;
            db_o_1 = db_o_1 + da_o_1;
            dw_c_1 = dw_c_1 + da_g_1 * seq[(int)(t_3)];
            du_c_1 = du_c_1 + da_g_1 * h_prev_3;
            db_c_1 = db_c_1 + da_g_1;
            dh_next_1 = da_i_1 * w.u_i + da_f_1 * w.u_f + da_o_1 * w.u_o + da_g_1 * w.u_c;
            dc_next_1 = dc_1 * f_t_3;
            t_3 = t_3 - 1;
        }
w.w_y = w.w_y - lr * dw_y_1;
w.b_y = w.b_y - lr * db_y_1;
w.w_i = w.w_i - lr * dw_i;
w.u_i = w.u_i - lr * du_i_1;
w.b_i = w.b_i - lr * db_i_1;
w.w_f = w.w_f - lr * dw_f_1;
w.u_f = w.u_f - lr * du_f_1;
w.b_f = w.b_f - lr * db_f_1;
w.w_o = w.w_o - lr * dw_o_1;
w.u_o = w.u_o - lr * du_o_1;
w.b_o = w.b_o - lr * db_o_1;
w.w_c = w.w_c - lr * dw_c_1;
w.u_c = w.u_c - lr * du_c_1;
w.b_c = w.b_c - lr * db_c_1;
        return w;
    }

    static Samples make_samples(double[] data, long look_back) {
        double[][] X = ((double[][])(new double[][]{}));
        double[] Y_1 = ((double[])(new double[]{}));
        long i_1 = 0;
        while (i_1 + look_back < data.length) {
            double[] seq_1 = ((double[])(java.util.Arrays.copyOfRange(data, (int)(i_1), (int)(i_1 + look_back))));
            X = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(X), java.util.stream.Stream.of(seq_1)).toArray(double[][]::new)));
            Y_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(Y_1), java.util.stream.DoubleStream.of(data[(int)(i_1 + look_back)])).toArray()));
            i_1 = i_1 + 1;
        }
        return new Samples(X, Y_1);
    }

    static LSTMWeights init_weights() {
        return new LSTMWeights(0.1, 0.2, 0.0, 0.1, 0.2, 0.0, 0.1, 0.2, 0.0, 0.1, 0.2, 0.0, 0.1, 0.0);
    }

    static LSTMWeights train(double[] data, long look_back, long epochs, double lr) {
        Samples samples = make_samples(((double[])(data)), look_back);
        LSTMWeights w_1 = init_weights();
        long ep_1 = 0;
        while (ep_1 < epochs) {
            long j_1 = 0;
            while (j_1 < samples.x.length) {
                double[] seq_3 = ((double[])(samples.x[(int)(j_1)]));
                double target_1 = samples.y[(int)(j_1)];
                LSTMState state_1 = forward(((double[])(seq_3)), w_1);
                w_1 = backward(((double[])(seq_3)), target_1, w_1, state_1, lr);
                j_1 = j_1 + 1;
            }
            ep_1 = ep_1 + 1;
        }
        return w_1;
    }

    static double predict(double[] seq, LSTMWeights w) {
        LSTMState state_2 = forward(((double[])(seq)), w);
        double h_last_3 = state_2.h[(int)(state_2.h.length - 1)];
        return w.w_y * h_last_3 + w.b_y;
    }
    public static void main(String[] args) {
        data = ((double[])(new double[]{0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8}));
        look_back = 3;
        epochs = 200;
        lr = 0.1;
        w_2 = train(((double[])(data)), look_back, epochs, lr);
        test_seq = ((double[])(new double[]{0.6, 0.7, 0.8}));
        pred = predict(((double[])(test_seq)), w_2);
        System.out.println("Predicted value: " + _p(pred));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
