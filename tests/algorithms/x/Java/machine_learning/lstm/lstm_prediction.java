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

    static double[] data = ((double[])(new double[]{0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8}));
    static long look_back = 3L;
    static long epochs = 200L;
    static double lr = (double)(0.1);
    static LSTMWeights w_2;
    static double[] test_seq = ((double[])(new double[]{0.6, 0.7, 0.8}));
    static double pred;

    static double exp_approx(double x) {
        double sum = (double)(1.0);
        double term_1 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 20L) {
            term_1 = (double)((double)((double)(term_1) * (double)(x)) / (double)((((Number)(n_1)).doubleValue())));
            sum = (double)((double)(sum) + (double)(term_1));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return sum;
    }

    static double sigmoid(double x) {
        return (double)(1.0) / (double)(((double)(1.0) + (double)(exp_approx((double)(-x)))));
    }

    static double tanh_approx(double x) {
        double e = (double)(exp_approx((double)((double)(2.0) * (double)(x))));
        return (double)(((double)(e) - (double)(1.0))) / (double)(((double)(e) + (double)(1.0)));
    }

    static LSTMState forward(double[] seq, LSTMWeights w) {
        double[] i_arr = ((double[])(new double[]{}));
        double[] f_arr_1 = ((double[])(new double[]{}));
        double[] o_arr_1 = ((double[])(new double[]{}));
        double[] g_arr_1 = ((double[])(new double[]{}));
        double[] c_arr_1 = ((double[])(new double[]{0.0}));
        double[] h_arr_1 = ((double[])(new double[]{0.0}));
        long t_1 = 0L;
        while ((long)(t_1) < (long)(seq.length)) {
            double x_1 = (double)(seq[(int)((long)(t_1))]);
            double h_prev_1 = (double)(h_arr_1[(int)((long)(t_1))]);
            double c_prev_1 = (double)(c_arr_1[(int)((long)(t_1))]);
            double i_t_1 = (double)(sigmoid((double)((double)((double)((double)(w.w_i) * (double)(x_1)) + (double)((double)(w.u_i) * (double)(h_prev_1))) + (double)(w.b_i))));
            double f_t_1 = (double)(sigmoid((double)((double)((double)((double)(w.w_f) * (double)(x_1)) + (double)((double)(w.u_f) * (double)(h_prev_1))) + (double)(w.b_f))));
            double o_t_1 = (double)(sigmoid((double)((double)((double)((double)(w.w_o) * (double)(x_1)) + (double)((double)(w.u_o) * (double)(h_prev_1))) + (double)(w.b_o))));
            double g_t_1 = (double)(tanh_approx((double)((double)((double)((double)(w.w_c) * (double)(x_1)) + (double)((double)(w.u_c) * (double)(h_prev_1))) + (double)(w.b_c))));
            double c_t_1 = (double)((double)((double)(f_t_1) * (double)(c_prev_1)) + (double)((double)(i_t_1) * (double)(g_t_1)));
            double h_t_1 = (double)((double)(o_t_1) * (double)(tanh_approx((double)(c_t_1))));
            i_arr = ((double[])(appendDouble(i_arr, (double)(i_t_1))));
            f_arr_1 = ((double[])(appendDouble(f_arr_1, (double)(f_t_1))));
            o_arr_1 = ((double[])(appendDouble(o_arr_1, (double)(o_t_1))));
            g_arr_1 = ((double[])(appendDouble(g_arr_1, (double)(g_t_1))));
            c_arr_1 = ((double[])(appendDouble(c_arr_1, (double)(c_t_1))));
            h_arr_1 = ((double[])(appendDouble(h_arr_1, (double)(h_t_1))));
            t_1 = (long)((long)(t_1) + 1L);
        }
        return new LSTMState(i_arr, f_arr_1, o_arr_1, g_arr_1, c_arr_1, h_arr_1);
    }

    static LSTMWeights backward(double[] seq, double target, LSTMWeights w, LSTMState s, double lr) {
        double dw_i = (double)(0.0);
        double du_i_1 = (double)(0.0);
        double db_i_1 = (double)(0.0);
        double dw_f_1 = (double)(0.0);
        double du_f_1 = (double)(0.0);
        double db_f_1 = (double)(0.0);
        double dw_o_1 = (double)(0.0);
        double du_o_1 = (double)(0.0);
        double db_o_1 = (double)(0.0);
        double dw_c_1 = (double)(0.0);
        double du_c_1 = (double)(0.0);
        double db_c_1 = (double)(0.0);
        double dw_y_1 = (double)(0.0);
        double db_y_1 = (double)(0.0);
        long T_1 = (long)(seq.length);
        double h_last_1 = (double)(s.h[(int)((long)(T_1))]);
        double y_1 = (double)((double)((double)(w.w_y) * (double)(h_last_1)) + (double)(w.b_y));
        double dy_1 = (double)((double)(y_1) - (double)(target));
        dw_y_1 = (double)((double)(dy_1) * (double)(h_last_1));
        db_y_1 = (double)(dy_1);
        double dh_next_1 = (double)((double)(dy_1) * (double)(w.w_y));
        double dc_next_1 = (double)(0.0);
        long t_3 = (long)((long)(T_1) - 1L);
        while ((long)(t_3) >= 0L) {
            double i_t_3 = (double)(s.i[(int)((long)(t_3))]);
            double f_t_3 = (double)(s.f[(int)((long)(t_3))]);
            double o_t_3 = (double)(s.o[(int)((long)(t_3))]);
            double g_t_3 = (double)(s.g[(int)((long)(t_3))]);
            double c_t_3 = (double)(s.c[(int)((long)((long)(t_3) + 1L))]);
            double c_prev_3 = (double)(s.c[(int)((long)(t_3))]);
            double h_prev_3 = (double)(s.h[(int)((long)(t_3))]);
            double tanh_c_1 = (double)(tanh_approx((double)(c_t_3)));
            double do_t_1 = (double)((double)(dh_next_1) * (double)(tanh_c_1));
            double da_o_1 = (double)((double)((double)(do_t_1) * (double)(o_t_3)) * (double)(((double)(1.0) - (double)(o_t_3))));
            double dc_1 = (double)((double)((double)((double)(dh_next_1) * (double)(o_t_3)) * (double)(((double)(1.0) - (double)((double)(tanh_c_1) * (double)(tanh_c_1))))) + (double)(dc_next_1));
            double di_t_1 = (double)((double)(dc_1) * (double)(g_t_3));
            double da_i_1 = (double)((double)((double)(di_t_1) * (double)(i_t_3)) * (double)(((double)(1.0) - (double)(i_t_3))));
            double dg_t_1 = (double)((double)(dc_1) * (double)(i_t_3));
            double da_g_1 = (double)((double)(dg_t_1) * (double)(((double)(1.0) - (double)((double)(g_t_3) * (double)(g_t_3)))));
            double df_t_1 = (double)((double)(dc_1) * (double)(c_prev_3));
            double da_f_1 = (double)((double)((double)(df_t_1) * (double)(f_t_3)) * (double)(((double)(1.0) - (double)(f_t_3))));
            dw_i = (double)((double)(dw_i) + (double)((double)(da_i_1) * (double)(seq[(int)((long)(t_3))])));
            du_i_1 = (double)((double)(du_i_1) + (double)((double)(da_i_1) * (double)(h_prev_3)));
            db_i_1 = (double)((double)(db_i_1) + (double)(da_i_1));
            dw_f_1 = (double)((double)(dw_f_1) + (double)((double)(da_f_1) * (double)(seq[(int)((long)(t_3))])));
            du_f_1 = (double)((double)(du_f_1) + (double)((double)(da_f_1) * (double)(h_prev_3)));
            db_f_1 = (double)((double)(db_f_1) + (double)(da_f_1));
            dw_o_1 = (double)((double)(dw_o_1) + (double)((double)(da_o_1) * (double)(seq[(int)((long)(t_3))])));
            du_o_1 = (double)((double)(du_o_1) + (double)((double)(da_o_1) * (double)(h_prev_3)));
            db_o_1 = (double)((double)(db_o_1) + (double)(da_o_1));
            dw_c_1 = (double)((double)(dw_c_1) + (double)((double)(da_g_1) * (double)(seq[(int)((long)(t_3))])));
            du_c_1 = (double)((double)(du_c_1) + (double)((double)(da_g_1) * (double)(h_prev_3)));
            db_c_1 = (double)((double)(db_c_1) + (double)(da_g_1));
            dh_next_1 = (double)((double)((double)((double)((double)(da_i_1) * (double)(w.u_i)) + (double)((double)(da_f_1) * (double)(w.u_f))) + (double)((double)(da_o_1) * (double)(w.u_o))) + (double)((double)(da_g_1) * (double)(w.u_c)));
            dc_next_1 = (double)((double)(dc_1) * (double)(f_t_3));
            t_3 = (long)((long)(t_3) - 1L);
        }
w.w_y = (double)(w.w_y) - (double)((double)(lr) * (double)(dw_y_1));
w.b_y = (double)(w.b_y) - (double)((double)(lr) * (double)(db_y_1));
w.w_i = (double)(w.w_i) - (double)((double)(lr) * (double)(dw_i));
w.u_i = (double)(w.u_i) - (double)((double)(lr) * (double)(du_i_1));
w.b_i = (double)(w.b_i) - (double)((double)(lr) * (double)(db_i_1));
w.w_f = (double)(w.w_f) - (double)((double)(lr) * (double)(dw_f_1));
w.u_f = (double)(w.u_f) - (double)((double)(lr) * (double)(du_f_1));
w.b_f = (double)(w.b_f) - (double)((double)(lr) * (double)(db_f_1));
w.w_o = (double)(w.w_o) - (double)((double)(lr) * (double)(dw_o_1));
w.u_o = (double)(w.u_o) - (double)((double)(lr) * (double)(du_o_1));
w.b_o = (double)(w.b_o) - (double)((double)(lr) * (double)(db_o_1));
w.w_c = (double)(w.w_c) - (double)((double)(lr) * (double)(dw_c_1));
w.u_c = (double)(w.u_c) - (double)((double)(lr) * (double)(du_c_1));
w.b_c = (double)(w.b_c) - (double)((double)(lr) * (double)(db_c_1));
        return w;
    }

    static Samples make_samples(double[] data, long look_back) {
        double[][] X = ((double[][])(new double[][]{}));
        double[] Y_1 = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)((long)(i_1) + (long)(look_back)) < (long)(data.length)) {
            double[] seq_1 = ((double[])(java.util.Arrays.copyOfRange(data, (int)((long)(i_1)), (int)((long)((long)(i_1) + (long)(look_back))))));
            X = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(X), java.util.stream.Stream.of(new double[][]{seq_1})).toArray(double[][]::new)));
            Y_1 = ((double[])(appendDouble(Y_1, (double)(data[(int)((long)((long)(i_1) + (long)(look_back)))]))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return new Samples(X, Y_1);
    }

    static LSTMWeights init_weights() {
        return new LSTMWeights(0.1, 0.2, 0.0, 0.1, 0.2, 0.0, 0.1, 0.2, 0.0, 0.1, 0.2, 0.0, 0.1, 0.0);
    }

    static LSTMWeights train(double[] data, long look_back, long epochs, double lr) {
        Samples samples = make_samples(((double[])(data)), (long)(look_back));
        LSTMWeights w_1 = init_weights();
        long ep_1 = 0L;
        while ((long)(ep_1) < (long)(epochs)) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)(samples.x.length)) {
                double[] seq_3 = ((double[])(samples.x[(int)((long)(j_1))]));
                double target_1 = (double)(samples.y[(int)((long)(j_1))]);
                LSTMState state_1 = forward(((double[])(seq_3)), w_1);
                w_1 = backward(((double[])(seq_3)), (double)(target_1), w_1, state_1, (double)(lr));
                j_1 = (long)((long)(j_1) + 1L);
            }
            ep_1 = (long)((long)(ep_1) + 1L);
        }
        return w_1;
    }

    static double predict(double[] seq, LSTMWeights w) {
        LSTMState state_2 = forward(((double[])(seq)), w);
        double h_last_3 = (double)(state_2.h[(int)((long)((long)(state_2.h.length) - 1L))]);
        return (double)((double)(w.w_y) * (double)(h_last_3)) + (double)(w.b_y);
    }
    public static void main(String[] args) {
        w_2 = train(((double[])(data)), (long)(look_back), (long)(epochs), (double)(lr));
        pred = (double)(predict(((double[])(test_seq)), w_2));
        System.out.println("Predicted value: " + _p(pred));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
