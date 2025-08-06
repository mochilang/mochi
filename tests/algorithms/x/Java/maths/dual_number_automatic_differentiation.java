public class Main {
    static class Dual {
        double real;
        double[] duals;
        Dual(double real, double[] duals) {
            this.real = real;
            this.duals = duals;
        }
        Dual() {}
        @Override public String toString() {
            return String.format("{'real': %s, 'duals': %s}", String.valueOf(real), String.valueOf(duals));
        }
    }


    static Dual make_dual(double real, int rank) {
        double[] ds = ((double[])(new double[]{}));
        int i = 0;
        while (i < rank) {
            ds = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ds), java.util.stream.DoubleStream.of(1.0)).toArray()));
            i = i + 1;
        }
        return new Dual(real, ds);
    }

    static Dual dual_from_list(double real, double[] ds) {
        return new Dual(real, ds);
    }

    static Dual dual_add(Dual a, Dual b) {
        double[] s_dual = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < a.duals.length) {
            s_dual = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(s_dual), java.util.stream.DoubleStream.of(a.duals[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        double[] o_dual = ((double[])(new double[]{}));
        int j = 0;
        while (j < b.duals.length) {
            o_dual = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(o_dual), java.util.stream.DoubleStream.of(b.duals[j])).toArray()));
            j = j + 1;
        }
        if (s_dual.length > o_dual.length) {
            int diff = s_dual.length - o_dual.length;
            int k = 0;
            while (k < diff) {
                o_dual = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(o_dual), java.util.stream.DoubleStream.of(1.0)).toArray()));
                k = k + 1;
            }
        } else         if (s_dual.length < o_dual.length) {
            int diff2 = o_dual.length - s_dual.length;
            int k2 = 0;
            while (k2 < diff2) {
                s_dual = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(s_dual), java.util.stream.DoubleStream.of(1.0)).toArray()));
                k2 = k2 + 1;
            }
        }
        double[] new_duals = ((double[])(new double[]{}));
        int idx = 0;
        while (idx < s_dual.length) {
            new_duals = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(new_duals), java.util.stream.DoubleStream.of(s_dual[idx] + o_dual[idx])).toArray()));
            idx = idx + 1;
        }
        return new Dual(a.real + b.real, new_duals);
    }

    static Dual dual_add_real(Dual a, double b) {
        double[] ds_1 = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < a.duals.length) {
            ds_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ds_1), java.util.stream.DoubleStream.of(a.duals[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        return new Dual(a.real + b, ds_1);
    }

    static Dual dual_mul(Dual a, Dual b) {
        int new_len = a.duals.length + b.duals.length + 1;
        double[] new_duals_1 = ((double[])(new double[]{}));
        int idx_1 = 0;
        while (idx_1 < new_len) {
            new_duals_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(new_duals_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
            idx_1 = idx_1 + 1;
        }
        int i_3 = 0;
        while (i_3 < a.duals.length) {
            int j_1 = 0;
            while (j_1 < b.duals.length) {
                int pos = i_3 + j_1 + 1;
                double val = new_duals_1[pos] + a.duals[i_3] * b.duals[j_1];
new_duals_1[pos] = val;
                j_1 = j_1 + 1;
            }
            i_3 = i_3 + 1;
        }
        int k_1 = 0;
        while (k_1 < a.duals.length) {
            double val_1 = new_duals_1[k_1] + a.duals[k_1] * b.real;
new_duals_1[k_1] = val_1;
            k_1 = k_1 + 1;
        }
        int l = 0;
        while (l < b.duals.length) {
            double val_2 = new_duals_1[l] + b.duals[l] * a.real;
new_duals_1[l] = val_2;
            l = l + 1;
        }
        return new Dual(a.real * b.real, new_duals_1);
    }

    static Dual dual_mul_real(Dual a, double b) {
        double[] ds_2 = ((double[])(new double[]{}));
        int i_4 = 0;
        while (i_4 < a.duals.length) {
            ds_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ds_2), java.util.stream.DoubleStream.of(a.duals[i_4] * b)).toArray()));
            i_4 = i_4 + 1;
        }
        return new Dual(a.real * b, ds_2);
    }

    static Dual dual_pow(Dual x, int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("power must be a positive integer"));
        }
        if (n == 0) {
            return new Dual(1.0, new double[]{});
        }
        Dual res = x;
        int i_5 = 1;
        while (i_5 < n) {
            res = dual_mul(res, x);
            i_5 = i_5 + 1;
        }
        return res;
    }

    static double factorial(int n) {
        double res_1 = 1.0;
        int i_6 = 2;
        while (i_6 <= n) {
            res_1 = res_1 * (((Number)(i_6)).doubleValue());
            i_6 = i_6 + 1;
        }
        return res_1;
    }

    static double differentiate(java.util.function.Function<Dual,Dual> func, double position, int order) {
        Dual d = make_dual(position, 1);
        Dual result = func.apply(d);
        if (order == 0) {
            return result.real;
        }
        return result.duals[order - 1] * factorial(order);
    }

    static void test_differentiate() {
        java.util.function.Function<Dual,Dual>[] f1 = new java.util.function.Function[1];
        f1[0] = (x) -> dual_pow(x, 2);
        if (differentiate(f1[0], 2.0, 2) != 2.0) {
            throw new RuntimeException(String.valueOf("f1 failed"));
        }
        java.util.function.Function<Dual,Dual>[] f2 = new java.util.function.Function[1];
        f2[0] = (x_1) -> dual_mul(dual_pow(x_1, 2), dual_pow(x_1, 4));
        if (differentiate(f2[0], 9.0, 2) != 196830.0) {
            throw new RuntimeException(String.valueOf("f2 failed"));
        }
        java.util.function.Function<Dual,Dual>[] f3 = new java.util.function.Function[1];
        f3[0] = (y) -> dual_mul_real(dual_pow(dual_add_real(y, 3.0), 6), 0.5);
        if (differentiate(f3[0], 3.5, 4) != 7605.0) {
            throw new RuntimeException(String.valueOf("f3 failed"));
        }
        java.util.function.Function<Dual,Dual>[] f4 = new java.util.function.Function[1];
        f4[0] = (y_1) -> dual_pow(y_1, 2);
        if (differentiate(f4[0], 4.0, 3) != 0.0) {
            throw new RuntimeException(String.valueOf("f4 failed"));
        }
    }

    static void main() {
        test_differentiate();
        java.util.function.Function<Dual,Dual>[] f = new java.util.function.Function[1];
        f[0] = (y_2) -> dual_mul(dual_pow(y_2, 2), dual_pow(y_2, 4));
        double res_2 = differentiate(f[0], 9.0, 2);
        System.out.println(res_2);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
