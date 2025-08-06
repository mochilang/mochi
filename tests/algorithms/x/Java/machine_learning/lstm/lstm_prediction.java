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
    static int look_back;
    static int epochs;
    static double lr;
    static LSTMWeights w_1;
    static double[] test_seq;
    static double pred;

    static double exp_approx(double x) {
        double sum = 1.0;
        double term = 1.0;
        int n = 1;
        while (n < 20) {
            term = term * x / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
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
        double[] f_arr = ((double[])(new double[]{}));
        double[] o_arr = ((double[])(new double[]{}));
        double[] g_arr = ((double[])(new double[]{}));
        double[] c_arr = ((double[])(new double[]{0.0}));
        double[] h_arr = ((double[])(new double[]{0.0}));
        int t = 0;
        while (t < seq.length) {
            double x = seq[t];
            double h_prev = h_arr[t];
            double c_prev = c_arr[t];
            double i_t = sigmoid(w.w_i * x + w.u_i * h_prev + w.b_i);
            double f_t = sigmoid(w.w_f * x + w.u_f * h_prev + w.b_f);
            double o_t = sigmoid(w.w_o * x + w.u_o * h_prev + w.b_o);
            double g_t = tanh_approx(w.w_c * x + w.u_c * h_prev + w.b_c);
            double c_t = f_t * c_prev + i_t * g_t;
            double h_t = o_t * tanh_approx(c_t);
            i_arr = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(i_arr), java.util.stream.DoubleStream.of(i_t)).toArray()));
            f_arr = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(f_arr), java.util.stream.DoubleStream.of(f_t)).toArray()));
            o_arr = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(o_arr), java.util.stream.DoubleStream.of(o_t)).toArray()));
            g_arr = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(g_arr), java.util.stream.DoubleStream.of(g_t)).toArray()));
            c_arr = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(c_arr), java.util.stream.DoubleStream.of(c_t)).toArray()));
            h_arr = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(h_arr), java.util.stream.DoubleStream.of(h_t)).toArray()));
            t = t + 1;
        }
        return new LSTMState(i_arr, f_arr, o_arr, g_arr, c_arr, h_arr);
    }

    static LSTMWeights backward(double[] seq, double target, LSTMWeights w, LSTMState s, double lr) {
        double dw_i = 0.0;
        double du_i = 0.0;
        double db_i = 0.0;
        double dw_f = 0.0;
        double du_f = 0.0;
        double db_f = 0.0;
        double dw_o = 0.0;
        double du_o = 0.0;
        double db_o = 0.0;
        double dw_c = 0.0;
        double du_c = 0.0;
        double db_c = 0.0;
        double dw_y = 0.0;
        double db_y = 0.0;
        int T = seq.length;
        double h_last = s.h[T];
        double y = w.w_y * h_last + w.b_y;
        double dy = y - target;
        dw_y = dy * h_last;
        db_y = dy;
        double dh_next = dy * w.w_y;
        double dc_next = 0.0;
        int t_1 = T - 1;
        while (t_1 >= 0) {
            double i_t_1 = s.i[t_1];
            double f_t_1 = s.f[t_1];
            double o_t_1 = s.o[t_1];
            double g_t_1 = s.g[t_1];
            double c_t_1 = s.c[t_1 + 1];
            double c_prev_1 = s.c[t_1];
            double h_prev_1 = s.h[t_1];
            double tanh_c = tanh_approx(c_t_1);
            double do_t = dh_next * tanh_c;
            double da_o = do_t * o_t_1 * (1.0 - o_t_1);
            double dc = dh_next * o_t_1 * (1.0 - tanh_c * tanh_c) + dc_next;
            double di_t = dc * g_t_1;
            double da_i = di_t * i_t_1 * (1.0 - i_t_1);
            double dg_t = dc * i_t_1;
            double da_g = dg_t * (1.0 - g_t_1 * g_t_1);
            double df_t = dc * c_prev_1;
            double da_f = df_t * f_t_1 * (1.0 - f_t_1);
            dw_i = dw_i + da_i * seq[t_1];
            du_i = du_i + da_i * h_prev_1;
            db_i = db_i + da_i;
            dw_f = dw_f + da_f * seq[t_1];
            du_f = du_f + da_f * h_prev_1;
            db_f = db_f + da_f;
            dw_o = dw_o + da_o * seq[t_1];
            du_o = du_o + da_o * h_prev_1;
            db_o = db_o + da_o;
            dw_c = dw_c + da_g * seq[t_1];
            du_c = du_c + da_g * h_prev_1;
            db_c = db_c + da_g;
            dh_next = da_i * w.u_i + da_f * w.u_f + da_o * w.u_o + da_g * w.u_c;
            dc_next = dc * f_t_1;
            t_1 = t_1 - 1;
        }
w.w_y = w.w_y - lr * dw_y;
w.b_y = w.b_y - lr * db_y;
w.w_i = w.w_i - lr * dw_i;
w.u_i = w.u_i - lr * du_i;
w.b_i = w.b_i - lr * db_i;
w.w_f = w.w_f - lr * dw_f;
w.u_f = w.u_f - lr * du_f;
w.b_f = w.b_f - lr * db_f;
w.w_o = w.w_o - lr * dw_o;
w.u_o = w.u_o - lr * du_o;
w.b_o = w.b_o - lr * db_o;
w.w_c = w.w_c - lr * dw_c;
w.u_c = w.u_c - lr * du_c;
w.b_c = w.b_c - lr * db_c;
        return w;
    }

    static Samples make_samples(double[] data, int look_back) {
        double[][] X = ((double[][])(new double[][]{}));
        double[] Y = ((double[])(new double[]{}));
        int i = 0;
        while (i + look_back < data.length) {
            double[] seq = ((double[])(java.util.Arrays.copyOfRange(data, i, i + look_back)));
            X = ((double[][])(appendObj(X, seq)));
            Y = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(Y), java.util.stream.DoubleStream.of(data[i + look_back])).toArray()));
            i = i + 1;
        }
        return new Samples(X, Y);
    }

    static LSTMWeights init_weights() {
        return new LSTMWeights(0.1, 0.2, 0.0, 0.1, 0.2, 0.0, 0.1, 0.2, 0.0, 0.1, 0.2, 0.0, 0.1, 0.0);
    }

    static LSTMWeights train(double[] data, int look_back, int epochs, double lr) {
        Samples samples = make_samples(((double[])(data)), look_back);
        LSTMWeights w = init_weights();
        int ep = 0;
        while (ep < epochs) {
            int j = 0;
            while (j < samples.x.length) {
                double[] seq_1 = ((double[])(samples.x[j]));
                double target = samples.y[j];
                LSTMState state = forward(((double[])(seq_1)), w);
                w = backward(((double[])(seq_1)), target, w, state, lr);
                j = j + 1;
            }
            ep = ep + 1;
        }
        return w;
    }

    static double predict(double[] seq, LSTMWeights w) {
        LSTMState state_1 = forward(((double[])(seq)), w);
        double h_last_1 = state_1.h[state_1.h.length - 1];
        return w.w_y * h_last_1 + w.b_y;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            data = ((double[])(new double[]{0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8}));
            look_back = 3;
            epochs = 200;
            lr = 0.1;
            w_1 = train(((double[])(data)), look_back, epochs, lr);
            test_seq = ((double[])(new double[]{0.6, 0.7, 0.8}));
            pred = predict(((double[])(test_seq)), w_1);
            System.out.println("Predicted value: " + _p(pred));
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
        return String.valueOf(v);
    }
}
